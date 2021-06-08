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
    val cacheLineSize = 8 // Number of pages to be fetched from a PTW
    val ptSize = 128  // entries
    val pageSize = 32 //bits
    val pageOffset = log2Ceil(pageSize)
    val dataWidth = 32
    val tlbSize = 8
    val coalBits = 3
    val attrBits = 3
    val vpn_width=vMemAddressWidth - pageOffset
    val ppn_width=pMemAddressWidth - pageOffset
    val tlbEntryWidth = vpn_width + coalBits + attrBits + ppn_width
    // ========= CoLT-FA TLB entry structure =========
    // BaseVPN [vMemAddressWidth - pageOffset] | CoalLength[coalBits] | Attributes [attrBits] | Base PPN [pMemAddressWidth - pageOffset]
    val vpn_tlb_start        =tlbEntryWidth-1
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

        val retAddress = Decoupled(Output(UInt(ppn_width.W))) //Returns the desired PPN      
    })
    
    val previousRetAddressReg = RegNext(io.retAddress.bits, 0.U(ppn_width.W))
    val previousRetValid = RegNext (io.retAddress.valid, false.B)
    val reqVPN=WireDefault(0.U(vpn_width.W))
    val resultIndex = WireDefault(0.U(log2Ceil(tlbSize).W))
    val pteCounter = RegInit(0.U(log2Ceil(cacheLineSize).W))
    var pteIntCounter = 0
    val tlbHit = WireDefault(false.B)
    val ptwDone = WireDefault(false.B)
    val writeDone = WireDefault(false.B)
    val operationDone = WireDefault(false.B) 
    val cacheLineRegs = Reg(Vec(cacheLineSize,UInt(ppn_width.W)))
    val isCoalescable = WireDefault(false.B)
    val coalLength = RegInit(0.U(log2Ceil(coalBits).W))rrrW

    //printf("VpnWidth=%d PPNWidth=%d PageOffset=%d\n",vpn_width.U,ppn_width.U,pageOffset.U)

    // FSM Logic
    val idle :: lookup :: waitPTW :: fill :: Nil = Enum (4)
    val stateReg = RegInit(idle)
    switch (stateReg){
        is (idle){
            when (io.readEnable ) { stateReg:= lookup}
            .elsewhen(io.writeEnable ) {stateReg := fill}
        }
        is (lookup){
            when (!operationDone) {stateReg := waitPTW}
            .elsewhen(operationDone) { stateReg := idle }
        }
        is (waitPTW){
            when(ptwDone) { 
                //pteCounter:=0.U
                stateReg := fill
            }
        }
        is (fill){
            when (operationDone) {stateReg:=idle}
        }
    }
    
    //val coltEntriesRegs = RegInit(VecInit(Seq.fill(tlbSize)(0.U(tlbEntryWidth.W))))
    val coltEntriesRegs = RegInit(VecInit(2758715.U(tlbEntryWidth.W), // VPN=168 CoalLength=3 PPN=59
                                          755731.U(tlbEntryWidth.W), // VPN=46 CoalLength=1 PPN=19
                                          3471468.U(tlbEntryWidth.W), // VPN=211 CoalLength=7 PPN=108
                                          0.U(tlbEntryWidth.W),
                                          2228302.U(tlbEntryWidth.W), // VPN=136 CoalLength=0 PPN=78
                                          0.U(tlbEntryWidth.W),
                                          0.U(tlbEntryWidth.W),
                                          0.U(tlbEntryWidth.W)))
    val validRegs = RegInit(VecInit(true.B,true.B,true.B,false.B,true.B,false.B,false.B,false.B))
    io.readAddress.ready := stateReg === idle  
    io.writeAddress.ready:= stateReg === idle || stateReg===waitPTW
    io.retAddress.valid := tlbHit && validRegs(resultIndex) || writeDone // add another flag for write-returned (PTW) values
    
    operationDone := tlbHit && validRegs(resultIndex) || writeDone
    val ptwCycleCounter = RegInit(0.U(2.W))
    ptwCycleCounter := Mux(stateReg===waitPTW && io.writeEnable && io.writeAddress.valid, Mux(ptwCycleCounter===1.U, 0.U, ptwCycleCounter+1.U), 0.U)
    pteCounter:= Mux(ptwDone, 0.U, Mux(ptwCycleCounter===1.U, pteCounter+1.U, pteCounter))
    
    ptwDone := (pteCounter === (cacheLineSize.U -1.U)) && ptwCycleCounter===1.U

    //val diffVec2=diffVec.toVector
    /*
    var basePPN = new ListBuffer[UInt]()
    val coalLengthList = new ListBuffer[UInt]()
    val targetIndexList = new ListBuffer[UInt]()
    val targetEntriesList = new ListBuffer[UInt]()
    */
    val basePPN = VecInit(Seq.fill(cacheLineSize-1)(0.U(ppn_width.W)))
    val coalLengthList = VecInit(Seq.fill(cacheLineSize-1)(0.U(coalBits.W)))
    val isBasePPN = VecInit(Seq.fill(cacheLineSize)(false.B))
    val isEntryEnd = VecInit(Seq.fill(cacheLineSize)(false.B))
    val targetIndexList = VecInit(Seq.fill(cacheLineSize-1)(false.B))
    val targetEntriesList = VecInit(Seq.fill(cacheLineSize-1)(0.U(ppn_width.W)))
    var tempCoalCounter = WireDefault(0.U(coalBits.W))
    var previousPPN = WireDefault(0.U(ppn_width.W))
    //var previousPPN = RegInit(0.U(ppn_width.W))

    when (stateReg===lookup) {
        printf("======== Entered lookup mode ========\n")
        reqVPN := getVPNfromVA(io.readAddress.bits)
        printf("\t reqVPN=%d\n", reqVPN)
        //=================== Range check logic ===================
        tlbHit := coltEntriesRegs.exists{      
            case x => (getVPNfromTLB(x) <= getVPNfromVA(io.readAddress.bits)) && 
                      (getVPNfromVA(io.readAddress.bits)<= getVPNfromTLB(x) + getCoalLengthFromTLB(x))
        }
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
        io.retAddress.bits := Mux(tlbHit && validRegs(resultIndex), finalRes, previousRetAddressReg)
        printf("\t valid=%d\n", io.retAddress.valid)
    }
    .elsewhen(stateReg===waitPTW){
        printf("======== Entered waitPTW mode ========\n")
        // Fill the cacheLineRegs (consecutive PTEs) Vector
        when (io.writeAddress.valid && io.writeEnable){
            cacheLineRegs(pteCounter) := io.writeAddress.bits
        }
        when (ptwCycleCounter===1.U){
            printf("\tPPN %d on write port\n", io.writeAddress.bits)
            printf("\tEntry %d is %d \n", pteCounter,cacheLineRegs(pteCounter))
        }
        
        io.retAddress.bits := previousRetAddressReg
        io.retAddress.valid:= false.B
    }
    .elsewhen (stateReg===fill) { 
        printf("======== Entered fill mode ========\n")
        
        for (i<-0 until cacheLineSize-1){
            when (cacheLineRegs(i+1)-cacheLineRegs(i)===1.U){
                targetEntriesList(i) := cacheLineRegs(i)
                targetIndexList(i) := true.B
            }
        }

        when (targetIndexList(0)) {
            isBasePPN(0) := true.B
        }

        for (i<-1 until cacheLineSize-1){
            when (!targetIndexList(i) && targetIndexList(i-1)){
                // End of current coalesced entry at i-1. Find the coalescing length.
                isEntryEnd(i) := true.B
            }.elsewhen(targetIndexList(i) && !targetIndexList(i-1)){
                // New Base PPN was found at i
                isBasePPN(i) := true.B
                when (i.U===cacheLineSize.U-2.U){
                    isBasePPN(i) := true.B
                    isEntryEnd(i+1) := true.B
                }
            }.elsewhen(targetIndexList(i) && targetIndexList(i-1)){
                // Increase the coalescing length counter
                when (i.U===(cacheLineSize.U-2.U)){
                    isEntryEnd(i+1) := true.B
                }
            }
        }
/*

        when (targetIndexList(0)) {
            basePPN(0):= targetEntriesList(0)
            previousPPN:= targetEntriesList(0)
            isBasePPN(0) := true.B
        }
        printf("Before for loop:\n")
        for (i<-0 until cacheLineSize-1){
            printf("TargetEntry(%d)=%d \n",i.U,targetEntriesList(i))
        }

        printf("Before for loop: previousPPN=%d \n", previousPPN)
        
        for (i<-1 until cacheLineSize-1){
            //printf("target Entry%d =%d\n",i.U,targetEntriesList(i))
            when (!targetIndexList(i) && targetIndexList(i-1)){
                // End of current coalesced entry at i-1. Find the coalescing length.
                printf("First when says: i=%d, target entry=%d, previousPPN=%d \n",i.U,targetEntriesList(i-1), previousPPN)
                coalLengthList(i-1) := 1.U + targetEntriesList(i-1) - previousPPN
                isEntryEnd(i) := true.B
            }.elsewhen(targetIndexList(i) && !targetIndexList(i-1)){
                printf("Second when says (before execution): i=%d, target entry=%d, previousPPN=%d \n",i.U,targetEntriesList(i-1), previousPPN)
                // New Base PPN was found at i
                basePPN(i) := targetEntriesList(i)
                previousPPN := targetEntriesList(i)
                isBasePPN(i) := true.B
                //tempCoalCounter := 0.U
                when (i.U===cacheLineSize.U-2.U){
                    coalLengthList(i):=1.U
                    isBasePPN(i) := true.B
                    isEntryEnd(i+1) := true.B
                }
                printf("Second when says (after execution): i=%d, target entry=%d, previousPPN=%d \n",i.U,targetEntriesList(i-1), previousPPN)
            }.elsewhen(targetIndexList(i) && targetIndexList(i-1)){
                // Increase the coalescing length counter
                //tempCoalCounter := tempCoalCounter + 1.U
                printf("Third when says (before execution): i=%d, target entry=%d, previousPPN=%d \n",i.U,targetEntriesList(i-1), previousPPN)
                when (i.U===(cacheLineSize.U-2.U)){
                    coalLengthList(i):=targetEntriesList(i)-previousPPN + 1.U
                    isEntryEnd(i+1) := true.B
                    printf("Got here\n")
                }
                printf("Third when says (after execution): i=%d, target entry=%d, previousPPN=%d \n",i.U,targetEntriesList(i-1), previousPPN)
            }
           // printf("i=%d previousPPN=%d\n",i.U,previousPPN)
        }
        //printf("Before for loop: previousPPN=%d \n", previousPPN)
        for (i<-0 until cacheLineSize-1){
            printf("Entry%d: Base PPN =%d, Coal Length=%d\n",i.U,basePPN(i),coalLengthList(i))
            printf ("Target Index %d \n", targetIndexList(i))
        }
*/
        printf("\n===== Results =====\n")
        for (i<-0 until cacheLineSize){
            printf("Entry%d (PPN=%d): isEntryEnd=%d isBasePPN=%d\n",i.U,cacheLineRegs(i), isEntryEnd(i), isBasePPN(i))
        }

        //writeDone:=true.B
        io.retAddress.bits := previousRetAddressReg
        io.retAddress.valid:= true.B
    }
    .otherwise{ 
        printf("======== Entered idle mode ========\n")
        io.retAddress.bits:=previousRetAddressReg
        io.retAddress.valid:=previousRetValid
    }


    def vmask (entry: UInt): UInt= {entry(vpn_width-1,coalBits)}
    def pmask (entry: UInt): UInt= {entry(ppn_width-1,coalBits)}
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