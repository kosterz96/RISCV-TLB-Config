package CoLT

import chisel3._
import chisel3.util._
import chisel3.testers._
//import chisel3.testers.RawTester.test

// import CoLT.Params

object CoLT_FA extends App{
    //chisel3.Driver.execute(Array[String](), () => new CoLT_FA)
    //val arguments = new Array[String](5)
    chisel3.stage.ChiselStage.elaborate(new CoLT_FA)
}

class CoLT_FA () extends Module {
    val pMemSize = 128
    val pMemAddressWidth = log2Ceil(pMemSize)
    val vMemSize = 256
    val vMemAddressWidth: Int = log2Ceil(vMemSize)
    val cacheLineSize = 4 // Number of pages to be fetched from a PTW
    val ptSize = 128  // 2^7
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
        val readAddress = Input (UInt(vMemAddressWidth.W))  // Determines the requested VIRTUAL address
        val readEnable = Input(Bool())                      // Determines whether or not read operation is allowed
        
        // Incoming write operations indicate that a page table walk has been conducted
        // The "write Address" field indicates the page that was found
        // and brought into the CoLT-FA TLB to be stored
        val writeAddress = Input (UInt((cacheLineSize*pMemAddressWidth).W)) 
        val writeEnable = Input(Bool())
        val writeData = Input (UInt(dataWidth.W)) //maybe useless
        
        val retData = Output (UInt(dataWidth.W))    // Provides the requested TLB entry
        val validData = Output (Bool())             // Ensures that the TLB entry provided to the consumer
                                                    // is valid (it was found among the entries)
                                                    // and it's not just the previous return address. Check lookup.
        val retAddress = Output (UInt(ppn_width.W)) //Returns the desired PPN
    })
    
    val previousRetAddressReg = RegNext(io.retAddress, 0.U(ppn_width.W))
    val reqVPN=Reg(UInt(vpn_width.W))
    val resultIndexReg = Reg(UInt(log2Ceil(tlbSize).W))
    val foundReg = RegInit(false.B)
    
    val pMem = SyncReadMem(pMemSize, UInt(dataWidth.W))
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
    // ============== END OF NOOOOOOOOOOB ===================

    
    when (io.readEnable) {
        reqVPN := getVPNfromVA(io.readAddress)
        //=================== Range check logic ===================
        foundReg := coltEntriesRegs.exists{case x => (getVPNfromTLB(x) <= getVPNfromVA(io.readAddress)) && (getVPNfromVA(io.readAddress)<= getVPNfromTLB(x) + getCoalLengthFromTLB(x))}
        // "foundReg" is set if the requested address matches the range check logic
        
        resultIndexReg := coltEntriesRegs.indexWhere {case x => (getVPNfromTLB(x) <= getVPNfromVA(io.readAddress)) && (getVPNfromVA(io.readAddress)<= getVPNfromTLB(x) + getCoalLengthFromTLB(x))}
        // "resultIndexReg" stores the index of the TLB entry that matched the request
        // If the requested TLB entry was not found (TLB miss), then "result" value is 0.U
        
        //=================== PPN Generation Logic ===================
        val finalRes = getVPNfromVA(io.readAddress) - getVPNfromTLB(coltEntriesRegs(resultIndexReg)) + getPPNfromTLB(coltEntriesRegs(resultIndexReg))
        
        // Update-return operations
        io.retAddress := Mux(foundReg, finalRes, previousRetAddressReg)
        io.validData := foundReg
    }
    .elsewhen (io.writeEnable) { 
        /*
        val ppnVec = Reg(Vec(cacheLineSize,UInt(PPN_WIDTH.W)))
        // Spit PTE entries and extract PPN
        for (i<-0 until cacheLineSize){
            ppnVec.updated(i,getPPNfromPA(io.writeAddress(i*pMemAddressWidth+pMemAddressWidth-1,i*pMemAddressWidth)))
        }
        // At this point we have to check if reqVPN, reqVPN+1,..., correspond to consecutive PPNs
        // To do so we must traslate each VPN seperately
        */
        
        // ======= This is a NOOB write implementation just to test reads =======
        //var idx=randomUInt(tlbSize)
        //coltEntriesRegs(io.writeData)= io.writeAddress
        //coltEntriesRegs.updated(idx, io.writeAddress)
        //printf(s"Address ${io.writeAddress} was written. Index=$idx\n")
        // ============= END OF NOOB IMPLEMENTATION =============================
        
        io.retAddress := previousRetAddressReg
        io.validData:= false.B
    }
    .otherwise { 
        io.retAddress:=previousRetAddressReg
        io.validData:=false.B
    }
    io.retData:= 0.U

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
}