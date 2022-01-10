namespace Day16

module Part1 = 
    open System
    open System.IO

    type PacketType = Literal | Operator | Unknown

    type ParseType = Header | Literal | Operator | Done

    type Packet = {
        Version: int
        Type: PacketType
        LiteralValue: int option
        SubPackets: List<Packet> option
    } with
        static member empty =
        {
            Version = 0
            Type = Unknown
            LiteralValue = None
            SubPackets = None
        }

    let parsePacketHeader (packetData:List<string>) =
        // Get the 6 bits and parse the version and type id info
        let v1::v2::v3::t1::t2::t3::rest = packetData
        let version = Convert.ToInt32(v1+v2+v3,2)
        let ptype = Convert.ToInt32(t1+t2+t3,2)
        let packetType =
            if (ptype = 4) then PacketType.Literal else PacketType.Operator
        let packet =
            {
                Version = version
                Type = packetType
                LiteralValue = None
                SubPackets = None
            }
        packet,rest

    let parsePacketLiteral (packetData:List<string>) (currentPacket:Packet) =
        let rec parseLiteral (data:List<string>) literalString =
            // read the 5 bits
            let l1::l2::l3::l4::l5::rest = data
            let part = l2+l3+l4+l5
            if (l1 = "1") then
                // extract current section and get next section
                parseLiteral rest (literalString + part)
            else
                // extract last part and return
                let literal = Convert.ToInt32(literalString + part,2)
                let packet = {currentPacket with LiteralValue = Some literal}
                packet, rest
        parseLiteral packetData String.Empty

    (*
    let rec ProcessFixedLengthPacket (packetData:List<string>) (currentPacket:Packet) versionSum =
        if (List.length packetData = 0) then
            currentPacket, versionSum
        else
            // Get the next packet info
            let newPacket, rest = parsePacketHeader packetData
            let newVersionSum = newPacket.Version + versionSum
            let subPackets = 
                currentPacket.SubPackets |> Option.defaultValue List.empty
                |> List.add newPacket
            let parentPacket = {currentPacket with SubPackets = subPackets}
    

    let parsePacketOperator (packetData:List<string>) (currentPacket:Packet) versionSum =
        if (List.length packetData = 0) then
            versionSum
        else
            let l1::rest = packetData
            // get the subpackets
            let subPackets = currentPacket.SubPackets |> Option.defaultValue List.empty
            if (l1 = "0") then
                // next 15 bits represent the lenght of the subpacket
                let subL, subPData = List.splitAt 15 rest
                let subLength = Convert.ToInt32(subL, 2)
                let subPacketData,_ = List.splitAt subLength subPData
                // Get the new 

            else
    
    let rec processPacket parseType (packetData:List<string>) packets versionSum =

        let packet, rest = parsePacketHeader packetData
        let newVersionSum = versionSum + packet.Version

        if (packet.Type = PacketType.Literal) then
            parsePacketLiteral rest packet
        else
    *)
    let solution inputFile =

        let hexMap = 
            [|
                ('0',"0000");
                ('1',"0001");
                ('2',"0010");
                ('3',"0011");
                ('4',"0100");
                ('5',"0101");
                ('6',"0110");
                ('7',"0111");
                ('8',"1000");
                ('9',"1001");
                ('A',"1010");
                ('B',"1011");
                ('C',"1100");
                ('D',"1101");
                ('E',"1110");
                ('F',"1111")            
            |]
            |> Map.ofArray

        let line = File.ReadAllText inputFile

        // transformed string
        let binLine =
            line.Trim().ToCharArray()
            |> Array.fold(fun s c -> 
                s + Map.find c hexMap
            )""


        ()