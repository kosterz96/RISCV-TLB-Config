package CoLT

import chisel3._
import chisel3.util._
import chisel3.testers._


object CoLT_FA extends App{
    //chisel3.Driver.execute(Array[String](), () => new CoLT_FA)
    //val arguments = new Array[String](5)
    chisel3.stage.ChiselStage.elaborate(new CoLT_FA())
}

class CoLT_FA () extends Module {
    val pMemSize = 128
    val pMemAddressWidth = log2Ceil(pMemSize)
    val vMemSize = 256
    val vMemAddressWidth: Int = log2Ceil(vMemSize)
    val cacheLineSize = 4 // Number of pages to be fetched from a PTW
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
        val writeAddress = Flipped(Decoupled(Input (UInt((cacheLineSize*ppn_width).W)))) // SOS!!!!! The incoming write requests are PTEs, not PPNs!
        val writeEnable = Input(Bool())

        val retAddress = Decoupled(Output(UInt(ppn_width.W))) //Returns the desired PPN

        /*
        val writeData = Flipped(Decoupled(Input (UInt(dataWidth.W)))) //maybe useless
        val retData = Output (UInt(dataWidth.W))    // Provides the requested TLB entry
        val validData = Output (Bool())             // Ensures that the TLB entry provided to the consumer
                                                    // is valid (it was found among the entries)
                                                    // and it's not just the previous return address. Check lookup.
        */
        
    })
    
    val previousRetAddressReg = RegNext(io.retAddress.bits, 0.U(ppn_width.W))
    val previousRetValid = RegNext (io.retAddress.valid, false.B)
    val reqVPN=WireDefault(0.U(vpn_width.W))
    val resultIndex = WireDefault(0.U(log2Ceil(tlbSize).W))
    val tlbHit = WireDefault(false.B)
    val operationDone = WireDefault(false.B) //probably useless
    //val operationDone = RegNext(io.retAddress.valid)
    val cacheLineRegs = Reg(Vec(cacheLineSize,UInt(ppn_width.W)))
    val isCoalescable = WireDefault(false.B)

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
            when(io.writeAddress.valid && io.writeEnable ) { stateReg := fill}
        }
        is (fill){
            when (operationDone) {stateReg:=idle}
        }
    }
    
    //io.readAddress.ready := stateReg === idle
    io.readAddress.ready := stateReg === idle  
    io.writeAddress.ready:= stateReg === idle 
    

    //val coltEntriesRegs = RegInit(VecInit(Seq.fill(tlbSize)(0.U(tlbEntryWidth.W))))
    // ========= NOOOOOOOOB =================
    val coltEntriesRegs = RegInit(VecInit(259.U(tlbEntryWidth.W),
                                          545.U(tlbEntryWidth.W),
                                          0.U(tlbEntryWidth.W),
                                          0.U(tlbEntryWidth.W),
                                          1058.U(tlbEntryWidth.W),
                                          1538.U(tlbEntryWidth.W),
                                          0.U(tlbEntryWidth.W),
                                          0.U(tlbEntryWidth.W)))
    val validRegs = RegInit(VecInit(true.B,true.B,false.B,false.B,true.B,true.B,false.B,false.B))
    // ============== END OF NOOOOOOOOOOB ===================

    io.retAddress.valid := tlbHit && validRegs(resultIndex)
    operationDone := tlbHit && validRegs(resultIndex)
    when (stateReg===lookup) {
        printf("======== Entered lookup mode ========\n")
        reqVPN := getVPNfromVA(io.readAddress.bits)
        printf("\t reqVPN=%d\n", reqVPN)
        //=================== Range check logic ===================
        tlbHit := coltEntriesRegs.exists{
            case x => (getVPNfromTLB(x) <= getVPNfromVA(io.readAddress.bits)) && 
                      (getVPNfromVA(io.readAddress.bits)<= getVPNfromTLB(x) + getCoalLengthFromTLB(x))
        }
        printf("\t tlbHit=%d\n", tlbHit)
        // "tlbHit" is set if the requested address matches the range check logic
        
        resultIndex := coltEntriesRegs.indexWhere {
            case x => (getVPNfromTLB(x) <= getVPNfromVA(io.readAddress.bits)) && 
            (getVPNfromVA(io.readAddress.bits)<= getVPNfromTLB(x) + getCoalLengthFromTLB(x))
        }
        printf("\t resultIndex=%d\n", resultIndex)
        // "resultIndex" stores the index of the TLB entry that matched the request
        // If the requested TLB entry was not found (TLB miss), then "result" value is 0.U
        
        //=================== PPN Generation Logic ===================
        val finalRes = getVPNfromVA(io.readAddress.bits) - getVPNfromTLB(coltEntriesRegs(resultIndex)) + getPPNfromTLB(coltEntriesRegs(resultIndex))
        printf("\t Final result: %d \n", finalRes)
        printf("\t operationDone=%d\n", operationDone)

        // Update-return operations
        io.retAddress.bits := Mux(tlbHit && validRegs(resultIndex), finalRes, previousRetAddressReg)
        //io.retAddress.valid := Mux(tlbHit && validRegs(resultIndex), tlbHit && validRegs(resultIndex), previousRetValid)
        printf("\t valid=%d\n", io.retAddress.valid)
        //operationDone := tlbHit && validRegs(resultIndex)
    }
    .elsewhen (stateReg===fill) { 
        // Spit PPNs. SOS!!!!!!!!: We need the PTE entries and then extract the PPN. See IO comments!!!!!!
        for (i<-0 until cacheLineSize){
            //cacheLineRegs.updated(i,getPPNfromPA(io.writeAddress.bits(i*pMemAddressWidth+pMemAddressWidth-1,i*pMemAddressWidth)))
            cacheLineRegs.updated(i,io.writeAddress.bits(i*ppn_width+ppn_width-1,i*ppn_width))
        }
        // At this point we have to check if reqVPN, reqVPN+1,..., correspond to consecutive PPNs
        // To do so we must traslate each VPN seperately (??????)
        
        
        /*
        // ======= This is a NOOB write implementation just to test reads =======
        var idx=randomUInt(tlbSize)
        coltEntriesRegs(io.writeData)= io.writeAddress
        coltEntriesRegs.updated(idx, io.writeAddress)
        printf(s"Address ${io.writeAddress} was written. Index=$idx\n")
         ============= END OF NOOB IMPLEMENTATION =============================
        */
        isCoalescable := cacheLineRegs.map(x=> coalCheck(reqVPN, getPPNfromTLB(x))).reduce(_&&_)

        io.retAddress.bits := 0.U
        io.retAddress.valid:= true.B
    }
    .otherwise{ 
        printf("======== Entered idle mode ========\n")
        io.retAddress.bits:=previousRetAddressReg
        //io.retAddress.valid:=false.B
        io.retAddress.valid:=previousRetValid
    }

    def randomUInt(upperLimit: Int)= scala.util.Random.nextInt(upperLimit).U

    def getVPNfromTLB (entry: UInt):UInt={
        entry(vpn_tlb_start, vpn_tlb_end)
    }
    def getVPNfromVA (entry: UInt):UInt={
        entry(vMemAddressWidth-1, vMemAddressWidth-vpn_width)
    }
    def getCoalLengthFromTLB (entry: UInt):UInt={
        entry(coal_length_start, coal_length_end)
    }
    def getAttrFromTLB (entry: UInt):UInt={
        entry(attributes_start, attributes_end)
    }
    def getPPNfromTLB (entry: UInt):UInt={
        entry(ppn_tlb_start, ppn_tlb_end)
    }
    def getPPNfromPA (entry: UInt):UInt={
        entry(pMemAddressWidth-1, pMemAddressWidth-ppn_width)
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
}