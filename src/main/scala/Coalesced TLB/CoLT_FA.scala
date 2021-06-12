package CoLT

import chisel3._
import chisel3.util._
import chisel3.testers._
import scala.collection.mutable.ListBuffer


object CoLT_FA extends App{
    chisel3.stage.ChiselStage.elaborate(new CoLT_FA())
}

class CoLT_FA () extends Module {
    val pMemSize = 8192
    val pMemAddressWidth = log2Ceil(pMemSize)
    val vMemSize = 8192
    val vMemAddressWidth: Int = log2Ceil(vMemSize)
    //val cacheLineSize = 8 // Number of pages to be fetched from a PTW
    val ptSize = 128  // entries
    val pageSize = 32 //bits
    val pageOffset = log2Ceil(pageSize)
    val dataWidth = 32
    val tlbSize = 8
    val coalBits = 3
    val attrBits = 3
    val vpn_width=vMemAddressWidth - pageOffset
    val ppn_width=pMemAddressWidth - pageOffset
    val regTLBentryWidth = vpn_width + ppn_width
    val coltEntryWidth = vpn_width + coalBits + attrBits + ppn_width
    // ========= CoLT-FA TLB entry structure =========
    // BaseVPN [vMemAddressWidth - pageOffset] | CoalLength[coalBits] | Attributes [attrBits] | Base PPN [pMemAddressWidth - pageOffset]
    val vpn_tlb_start        =coltEntryWidth-1
    val vpn_tlb_end          =vpn_tlb_start-vpn_width+1
    val coal_length_start    =vpn_tlb_end-1
    val coal_length_end      =coal_length_start-coalBits+1
    val attributes_start     =coal_length_end-1
    val attributes_end       =attributes_start-attrBits+1
    val ppn_tlb_start        =attributes_end-1
    val ppn_tlb_end          =ppn_tlb_start-ppn_width+1

    val io = IO(new Bundle {
        val readAddress = Flipped(Decoupled(Input (UInt(vMemAddressWidth.W))))  // Determines the requested VIRTUAL address
        val readEnable = Input(Bool())                      // Determines whether or not read operation is allowed
        
        // Incoming write operations indicate that a page table walk has been conducted
        // The "write Address" field indicates the page that was found
        // and brought into the CoLT-FA TLB to be stored
        val writeAddress = Flipped(Decoupled(Input (UInt((ppn_width).W)))) // SOS!!!!! The incoming write requests are PTEs, not PPNs!
        val writeEnable = Input(Bool())

        val fence_request = Input(Bool())
        val fenceAddress = Flipped(Valid(Input(UInt(vpn_width.W))))

        val retAddress = Decoupled(Output(UInt(ppn_width.W))) //Returns the desired PPN      
    })
    
    val previousRetAddressReg = RegNext(io.retAddress.bits, 0.U(ppn_width.W))
    val previousRetValid = RegNext (io.retAddress.valid, false.B)
    val reqVPN=RegInit(0.U(vpn_width.W))
    val resultIndex = WireDefault(0.U(log2Ceil(tlbSize).W))
    var pteIntCounter = 0
    val tlbHit = WireDefault(false.B)
    val ptwDone = WireDefault(false.B)
    val writeDone = WireDefault(false.B)
    val operationDone = WireDefault(false.B) 
    val isCoalescable = WireDefault(false.B)
    val coalLength = RegInit(0.U(log2Ceil(coalBits).W))
    val reqPPN = RegInit(0.U(ppn_width.W))

    //printf("VpnWidth=%d PPNWidth=%d PageOffset=%d\n",vpn_width.U,ppn_width.U,pageOffset.U)

    // FSM Logic
    val idle :: lookup :: waitPTW :: fill :: invalidate :: Nil = Enum (5)
    val stateReg = RegInit(idle)
    val previousStateReg = RegInit(idle)
    switch (stateReg){
        is (idle){
            when (io.fence_request){
                previousRetAddressReg := stateReg
                stateReg:=invalidate
            }.elsewhen(io.writeEnable) {
                stateReg := fill
            }.elsewhen (io.readEnable) { 
                stateReg:= lookup
            }
        }
        is (lookup){
            when (!operationDone) {stateReg := waitPTW}
            .elsewhen(operationDone) { stateReg := idle }
        }
        is (waitPTW){
            when (io.fence_request){
                previousRetAddressReg := stateReg
                stateReg:=invalidate
            }.elsewhen(ptwDone) { 
                stateReg := fill
            }
        }
        is (fill){
            when (operationDone) {stateReg:=idle}
            // Fence request cannot be operated during fill mode. A case scenario is when the fence request address is the same as the VPN/PPN to be filled.
        }
        is (invalidate){
            // todo

            // when the request is served
                stateReg := previousStateReg
        }
    }
    
    val regularTLBentries = RegInit(VecInit(Seq.fill(tlbSize)(0.U(regTLBentryWidth.W))))
    val regularValidRegs = RegInit(VecInit(false.B,false.B,false.B,false.B,false.B,false.B,false.B,false.B))

    val coltEntriesRegs = RegInit(VecInit(2758715.U(coltEntryWidth.W), // VPN=168 CoalLength=3 PPN=59
                                          755731.U(coltEntryWidth.W), // VPN=46 CoalLength=1 PPN=19
                                          3471468.U(coltEntryWidth.W), // VPN=211 CoalLength=7 PPN=108
                                          0.U(coltEntryWidth.W),
                                          2228302.U(coltEntryWidth.W), // VPN=136 CoalLength=0 PPN=78
                                          0.U(coltEntryWidth.W),
                                          0.U(coltEntryWidth.W),
                                          0.U(coltEntryWidth.W)))
    val coltValidRegs = RegInit(VecInit(true.B,true.B,true.B,false.B,true.B,false.B,false.B,false.B))
    io.readAddress.ready := stateReg === idle  
    io.writeAddress.ready:= stateReg === idle || stateReg===waitPTW
    io.retAddress.valid := tlbHit && coltValidRegs(resultIndex) || writeDone // add another flag for write-returned (PTW) values
    
    operationDone := tlbHit && coltValidRegs(resultIndex) || writeDone
    val ptwCycleCounter = RegInit(0.U(2.W))
    val coltMaskHit = WireDefault(false.B)
    val regularMaskHit = WireDefault(false.B)
    val coltMaskHitIndex = WireDefault(0.U(log2Ceil(tlbSize).W))
    val regularMaskHitIndex = WireDefault(0.U(log2Ceil(tlbSize).W))
    val regularHitEntry = WireDefault(0.U(regTLBentryWidth.W))
    val coltHitEntry = WireDefault(0.U(coltEntryWidth.W))
    val vpnHit = WireDefault(false.B)
    val invalidateIndex = WireDefault(0.U(ppn_width.W))
    ptwCycleCounter := Mux(stateReg===waitPTW && io.writeEnable && io.writeAddress.valid, Mux(ptwCycleCounter===1.U, 0.U, ptwCycleCounter+1.U), 0.U)
    ptwDone := ptwCycleCounter===1.U

   
    when (stateReg===lookup) {
        printf("======== Entered lookup mode ========\n")
        reqVPN := getVPNfromVA(io.readAddress.bits)
        printf("\t reqVPN=%d (this is the previous request but it doesn't really affect the operation)\n", reqVPN)
        printf ("\t Actually requested VPN=%d\n", getVPNfromVA(io.readAddress.bits))

        //=================== Range check logic ===================
        tlbHit := coltEntriesRegs.exists{      
            case x => (getVPNfromTLB(x) <= getVPNfromVA(io.readAddress.bits)) && 
                      (getVPNfromVA(io.readAddress.bits)<= getVPNfromTLB(x) + getCoalLengthFromTLB(x))
        }
        when (tlbHit){
            printf("\t tlbHit=%d\n", tlbHit)  // "tlbHit" is set if the requested address matches the range check logic
                
            resultIndex := coltEntriesRegs.indexWhere {
                case x => (getVPNfromTLB(x) <= getVPNfromVA(io.readAddress.bits)) && 
                (getVPNfromVA(io.readAddress.bits)<= getVPNfromTLB(x) + getCoalLengthFromTLB(x))
            }
            printf("\t resultIndex=%d\n", resultIndex)  // "resultIndex" stores the index of the TLB entry that matched the request
                            
            //=================== PPN Generation Logic ===================
            val finalRes = getVPNfromVA(io.readAddress.bits) - getVPNfromTLB(coltEntriesRegs(resultIndex)) + getPPNfromTLB(coltEntriesRegs(resultIndex))
            printf("\t Final result: %d \n", finalRes)
            printf("\t operationDone=%d\n", operationDone)

            // Update-return operations
            io.retAddress.bits := Mux(tlbHit && coltValidRegs(resultIndex), finalRes, previousRetAddressReg)
        
            printf("\t valid=%d\n", io.retAddress.valid)
        }.otherwise{ 
            printf("\t **** CoLT TLB miss ****\n")
            io.retAddress.bits := previousRetAddressReg
        }
        
    }
    .elsewhen(stateReg===waitPTW){
        printf("======== Entered waitPTW mode ========\n")

        when (io.writeAddress.valid && io.writeEnable){
            reqPPN := io.writeAddress.bits
            io.retAddress.bits := reqPPN
            io.retAddress.valid:= true.B
            when (ptwCycleCounter===1.U){
                printf("\tPPN %d on write port\n", io.writeAddress.bits)
                printf("\tRequested translation is VPN%d => PPN%d \n", reqVPN, reqPPN)
            }
        }.otherwise{
            io.retAddress.bits := previousRetAddressReg
            io.retAddress.valid:= false.B
        }
    }
    .elsewhen (stateReg===fill) { 
        printf("======== Entered fill mode ========\n")
        //coltMaskHit := (coltEntriesRegs zip coltValidRegs).toVector.exists{case (entry, flag) => flag && vmaskTLB(entry)===vmask(reqVPN) && pmaskTLB(entry)===pmask(reqPPN) }
        
        coltMaskHitIndex := coltEntriesRegs.indexWhere(entry=> vmaskTLB(entry)===vmask(reqVPN) && pmaskTLB(entry)===pmask(reqPPN))
        coltMaskHit := coltValidRegs(coltMaskHitIndex) && coltEntriesRegs.exists(entry => vmaskTLB(entry)===vmask(reqVPN) && pmaskTLB(entry)===pmask(reqPPN))
        
        regularMaskHitIndex:=regularTLBentries.indexWhere(entry=> vmask(entry(vpn_width+ppn_width-1,ppn_width))===vmask(reqVPN) && vmask(entry(ppn_width-1,0))===pmask(reqPPN))
        regularMaskHit := regularValidRegs(regularMaskHitIndex) && regularTLBentries.exists(entry=> vmask(entry(vpn_width+ppn_width-1,ppn_width))===vmask(reqVPN) && vmask(entry(ppn_width-1,0))===pmask(reqPPN))

        printf(" coltMaskHit=%d\n coltMaskHitIndex=%d\n regularMaskHit=%d\n regularMaskHitIndex=%d\n",coltMaskHit,coltMaskHitIndex,regularMaskHit,regularMaskHitIndex)


        /*
        when (!coltMaskHit){
            when (!regularMaskHit){
                // Fill the conventional TLB
                // TODO: also check valid bit 
                printf("No mask match for VPN%d\n", reqVPN)
                val idx=randomUInt(tlbSize)
                regularTLBentries(idx) := Cat(reqVPN,reqPPN)
                regularValidRegs(idx) := true.B
            }.otherwise{
                //Coalescing possibility. Check the ***** conventional TLB ***** entries using the offsets
                // TODO: Multiple mask hits in the conventional TLB.
                regularMaskHitIndex:=regularTLBentries.indexWhere(entry=> vmask(entry(vpn_width+ppn_width-1,ppn_width))===vmask(reqVPN) && vmask(entry(ppn_width-1,0))===pmask(reqPPN))
                regularHitEntry := regularTLBentries(regularMaskHitIndex)
                
                val idx2=randomUInt(tlbSize)
                printf("VPN%d mask matched in the conventional TLB\n Place it in CoLT with index=%d", reqVPN, idx2)

                when (voffset(reqVPN)-voffset(regularMaskHitIndex)===1.U && poffset(reqVPN)-poffset(regularMaskHitIndex)===1.U){
                    // Offset match. Fill CoLT TLB with a new entry
                    printf("New CoLT entry at index%d\n BaseVPN:%d BasePPN%d\n", idx2, reqVPN, reqPPN)
                    coltEntriesRegs(idx2) := Cat(reqVPN,1.U(coalBits.W), 0.U(attrBits.W), reqPPN)
                    coltValidRegs(idx2) := true.B
                    regularValidRegs (regularMaskHitIndex) := false.B
                }.elsewhen(voffset(regularMaskHitIndex)-voffset(reqVPN)===1.U && poffset(regularMaskHitIndex)-poffset(reqVPN)===1.U){
                    printf("New CoLT entry at index%d\n BaseVPN:%d BasePPN%d\n", idx2, regularHitEntry(vpn_width+ppn_width-1,ppn_width), regularHitEntry(ppn_width-1,0))
                    coltEntriesRegs(idx2) := Cat(regularHitEntry(vpn_width+ppn_width-1,ppn_width), 1.U(coalBits.W), 0.U(attrBits.W), regularHitEntry(ppn_width-1,0))
                    coltValidRegs(idx2) := true.B
                    regularValidRegs (regularMaskHitIndex) := false.B
                }
            }
        }.otherwise{
            when (!regularMaskHit){
                
                // Coalescing possibility. Check the ***** CoLT TLB ***** entries
                // TODO: Multiple mask hits in the CoLT TLB.
                coltMaskHitIndex := coltEntriesRegs.indexWhere(entry=> vmaskTLB(entry)===vmask(reqVPN) && pmaskTLB(entry)===pmask(reqPPN))
                coltHitEntry := coltEntriesRegs(coltMaskHitIndex)
                val idx3=randomUInt(tlbSize)
                
                when (voffset(reqVPN)- voffset(getVPNfromTLB(coltHitEntry)) === 1.U && poffset(reqVPN)-poffset(getVPNfromTLB(coltHitEntry))=== 1.U){
                    // Requested VPN is the new Base VPN in the entry (so is the PPN). Also increase the Coalescing Length by 1
                    printf("New CoLT entry at index%d\n BaseVPN:%d BasePPN%d\n", idx3, reqVPN, reqPPN)
                    coltEntriesRegs(coltMaskHit):= Cat(reqVPN, getCoalLengthFromTLB(coltHitEntry) + 1.U, getAttrFromTLB(coltHitEntry), reqPPN)
                    coltValidRegs(coltMaskHitIndex) := true.B //probably useless. valid bit is already set
                }.elsewhen(voffset(reqVPN) === voffset(getVPNfromTLB(coltHitEntry)) + getCoalLengthFromTLB(coltHitEntry) + 1.U && 
                        poffset(reqPPN) === poffset(getPPNfromTLB(coltHitEntry)) + getCoalLengthFromTLB(coltHitEntry) + 1.U){
                    // Requested VPN is just after the last entry (current Base VPN + current coal length + 1)
                    printf("New CoLT entry at index%d\n BaseVPN:%d BasePPN%d\n", idx3, getVPNfromTLB(coltHitEntry), getPPNfromTLB(coltHitEntry))
                    coltEntriesRegs(coltMaskHit) := Cat (getVPNfromTLB(coltHitEntry), getCoalLengthFromTLB(coltHitEntry) + 1.U, getAttrFromTLB(coltHitEntry), getPPNfromTLB(coltHitEntry))
                    coltValidRegs(coltMaskHitIndex) := true.B
                }
            }.otherwise{

                printf("Mask match in both TLBs.")
                printf("New CoLT entry at index (???)\n BaseVPN:%d BasePPN%d\n",  getVPNfromTLB(coltHitEntry), getPPNfromTLB(coltHitEntry))
            }
        }
*/        
        when (!regularMaskHit && !coltMaskHit){     // No mask match - Fill the conventional TLB
            // Fill the conventional TLB
            // TODO: also check valid bit 
            printf("No mask match for VPN%d\n", reqVPN)
            var idx=randomUInt(tlbSize)
            printf("New conventional entry at index%d\n", idx)
            regularTLBentries(idx) := Cat(reqVPN,reqPPN)
            regularValidRegs(idx) := true.B            
        }.elsewhen(regularMaskHit && regularValidRegs(regularMaskHitIndex) && !coltMaskHit){  //Coalescing possibility. Check the ***** conventional TLB ***** entries using the offsets
            // TODO: Multiple mask hits in the conventional TLB.
            regularHitEntry := regularTLBentries(regularMaskHitIndex)
            var idx2=randomUInt(tlbSize)
            printf("VPN%d mask matched in the conventional TLB\nPlace it in CoLT with index=%d\n", reqVPN, idx2)

            when (voffset(reqVPN)-voffset(regularMaskHitIndex)===1.U && poffset(reqVPN)-poffset(regularMaskHitIndex)===1.U){
                // Offset match. Fill CoLT TLB with a new entry
                printf("New CoLT entry at index%d\n BaseVPN:%d BasePPN%d\n", idx2, regularHitEntry(vpn_width+ppn_width-1,ppn_width), regularHitEntry(ppn_width-1,0))
                coltEntriesRegs(idx2) := Cat(regularHitEntry(vpn_width+ppn_width-1,ppn_width), 1.U(coalBits.W), 0.U(attrBits.W), regularHitEntry(ppn_width-1,0))
                coltValidRegs(idx2) := true.B
                regularValidRegs (regularMaskHitIndex) := false.B
            }.elsewhen(voffset(regularMaskHitIndex)-voffset(reqVPN)===1.U && poffset(regularMaskHitIndex)-poffset(reqVPN)===1.U){
                printf("New CoLT entry at index%d\n BaseVPN:%d BasePPN%d\n", idx2, reqVPN, reqPPN)
                coltEntriesRegs(idx2) := Cat(reqVPN,1.U(coalBits.W), 0.U(attrBits.W), reqPPN)
                coltValidRegs(idx2) := true.B
                regularValidRegs (regularMaskHitIndex) := false.B
            }
            
        }.elsewhen(!regularMaskHit && coltMaskHit && coltValidRegs(coltMaskHitIndex)){
            // Coalescing possibility. Check the ***** CoLT TLB ***** entries
            // TODO: Multiple mask hits in the CoLT TLB.
            coltHitEntry := coltEntriesRegs(coltMaskHitIndex)

            var idx3=randomUInt(tlbSize)
            when (voffset(getVPNfromTLB(coltHitEntry)) === voffset(reqVPN) + 1.U && poffset(getVPNfromTLB(coltHitEntry))=== poffset(reqVPN) + 1.U){
                // Requested VPN is the new Base VPN in the entry (so is the PPN). Also increase the Coalescing Length by 1
                printf("Update CoLT entry at index%d\n BaseVPN:%d BasePPN%d new Coalesing Length=%d\n", idx3, reqVPN, reqPPN, getCoalLengthFromTLB(coltHitEntry) + 1.U)
                coltEntriesRegs(coltMaskHit):= Cat(reqVPN, getCoalLengthFromTLB(coltHitEntry) + 1.U, getAttrFromTLB(coltHitEntry), reqPPN)
                coltValidRegs(coltMaskHitIndex) := true.B //probably useless. valid bit is already set


            }.elsewhen(voffset(reqVPN) === voffset(getVPNfromTLB(coltHitEntry)) + getCoalLengthFromTLB(coltHitEntry) + 1.U && 
                    poffset(reqPPN) === poffset(getPPNfromTLB(coltHitEntry)) + getCoalLengthFromTLB(coltHitEntry) + 1.U){
                // Requested VPN is just after the last entry (current Base VPN + current coal length + 1)
                printf("Update CoLT entry at index%d\n BaseVPN:%d BasePPN%d new Coalesing Length=%d\n", idx3, reqVPN, reqPPN, getCoalLengthFromTLB(coltHitEntry) + 1.U)
                coltEntriesRegs(coltMaskHit) := Cat (getVPNfromTLB(coltHitEntry), getCoalLengthFromTLB(coltHitEntry) + 1.U, getAttrFromTLB(coltHitEntry), getPPNfromTLB(coltHitEntry))
                coltValidRegs(coltMaskHitIndex) := true.B
            
            }
            

        }.elsewhen(regularMaskHit && coltMaskHit && regularValidRegs(regularMaskHitIndex) && coltValidRegs(coltMaskHitIndex)){
            // Mask matching in both TLBs ????
            printf("Mask match in both TLBs.")
            printf("New CoLT entry at index (???)\n BaseVPN:%d BasePPN%d\n",  getVPNfromTLB(coltHitEntry), getPPNfromTLB(coltHitEntry))
        }


        io.retAddress.bits := previousRetAddressReg
        io.retAddress.valid:= previousRetValid
        operationDone:=true.B
    }.elsewhen(stateReg===invalidate){
        // TODO
        invalidateIndex := coltEntriesRegs.indexWhere(entry => getVPNfromTLB(entry) === io.fenceAddress.bits)
        coltValidRegs (invalidateIndex) := false.B

        io.retAddress.bits:=previousRetAddressReg
        io.retAddress.valid:=previousRetValid
    }
    .otherwise{ 
        printf("======== Entered idle mode ========\n")

        printf(" *** CoLT entries *** \n")
        for (i<-0 until tlbSize){
            printf("Valid=%d Base VPN=%d, Base PPN=%d, Coalescing Length=%d\n", coltValidRegs(i), getVPNfromTLB(coltEntriesRegs(i)), getPPNfromTLB(coltEntriesRegs(i)), getCoalLengthFromTLB((coltEntriesRegs(i))))
        }

        for (i<-0 until tlbSize){
           // printf("Valid=%d VPN=%d, PPN=%d", regularValidRegs(i), )
        }

        io.retAddress.bits:=previousRetAddressReg
        io.retAddress.valid:=previousRetValid
    }


    def vmask (entry: UInt): UInt= {entry(vpn_width-1,coalBits) }
    def pmask (entry: UInt): UInt= {entry(ppn_width-1,coalBits) }
    def vmaskTLB (entry: UInt): UInt= {vmask(getVPNfromTLB(entry)) }
    def pmaskTLB (entry: UInt): UInt= {pmask(getPPNfromTLB(entry))}

    def voffset (entry: UInt): UInt= {entry(coalBits-1,0)}
    def poffset (entry: UInt): UInt= {entry(coalBits-1,0)}

    def coalCheck (request:UInt, CoLT_entry: UInt): Bool={
        vmask(request)===vmask(CoLT_entry) &&
        voffset(request)===(voffset(CoLT_entry)+1.U) &&
        voffset(CoLT_entry)+1.U <= math.pow(2,coalBits).toInt.U-1.U &&
        pmask(request) === pmask(CoLT_entry)
    }

    def randomUInt(upperLimit: Int)= scala.util.Random.nextInt(upperLimit).U

    def getVPNfromTLB (entry: UInt):UInt={entry(vpn_tlb_start, vpn_tlb_end)}
    def getVPNfromVA (entry: UInt):UInt={ entry(vMemAddressWidth-1, vMemAddressWidth-vpn_width) }
    def getCoalLengthFromTLB (entry: UInt):UInt={entry(coal_length_start, coal_length_end) }
    def getAttrFromTLB (entry: UInt):UInt={entry(attributes_start, attributes_end)}
    def getPPNfromTLB (entry: UInt):UInt={entry(ppn_tlb_start, ppn_tlb_end)}
    def getPPNfromPA (entry: UInt):UInt={entry(pMemAddressWidth-1, pMemAddressWidth-ppn_width)}    
}