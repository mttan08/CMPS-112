#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"Matthew Tan"
"mxtan"
"cs112"
"asg3: hzip.st"

nl := Character nl.
shift := 48.

FileStream extend [
   nl := Character nl.
   catFile [
      |count textline|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         textline :=self nextLine.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << textline << nl.
      ]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

Object subclass: Leaf [
   |char count isLeaf|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [^ isLeaf]
   
   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.      
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      isLeaf:=true.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   
   postorder: visitor [
      visitor  value:char value: isLeaf.
   ]
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      isLeaf:=false.
   ]

   left  [ ^ left]
   right [ ^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
   postorder: visitor[      
      left postorder: visitor .
      right postorder: visitor .  
      visitor value: char value:isLeaf.
   ]
]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^  self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
         myStream nextPutByte: currentByte.
         self clearByte.

   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [
        self flushByte].
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
]

|infile outfile argc|
argc := 0.
Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |
    (opt = $u) ifTrue: [ decompress:= true. ].
    (arg ~= nil) ifTrue: [
        argc = 0 ifTrue: [
            infile := FileStream open: arg
                        mode: FileStream read.
       ]
       ifFalse:[
           outfile := FileStream open: arg
                         mode: FileStream write.
       ].
       argc := argc + 1.
   ].
].

((argc < 1) | (argc > 3)) ifTrue: [
    stderr << 'Invalid arguments' << nl.
    stderr << 'Usage hzip.st -d '.
    stderr << '-t -c -u inputfile [outputfile]' << nl.
    ObjectMemory quit
].

priQueue := SortedCollection new.
Smalltalk arguments: '-d -c -t -u'
do: [:opt :arg |
    ((opt = $d) | (opt = $c) | (opt = $t)) ifTrue: [
        (opt = $d) ifTrue: [ stdout << 'create freqtable' << nl. ].
        freqtable := ZeroArray new: 257.
        freqtable at: 256 put: 1.
        [infile atEnd not] whileTrue: [
            aChar := infile next asInteger.
            freqtable incr: aChar.
        ].
        freqtable keysAndValuesDo: [:key :count |
           (count notNil and: [count > 0]) ifTrue: [
              priQueue add: (Leaf new: key count: count).
            ]
        ].

        (opt = $d) ifTrue: [ stdout << 'create priority queue' << nl. ].
        [priQueue size > 1 ] whileTrue: [
           |lchild rchild|
           lchild := priQueue removeFirst.
           rchild := priQueue removeFirst.
           newTree := (Tree new: (lchild char)
                         count: (lchild count) +  (rchild count)
                      left: lchild right: rchild).
            priQueue add: newTree.
        ].

        (opt = $d) ifTrue: [ stdout << 'create encode table' << nl. ].
        encodeTable := ZeroArray new: 257.
        tree := priQueue removeFirst.
        tree inorder: [:char :string |
            encodeTable at: char put: string.
        ] prefix: ''.

        "Compression"
        (opt = $c) ifTrue:[ 
            outfile ~= nil ifTrue:[
            outBytes := OutBits new: outfile.    
            tree postorder: [:char :isLeaf |
                isLeaf ifFalse: [
                   outBytes writeBit: ('1'  asInteger) - shift.
               ]
               ifTrue: [
                   bin := ''.
                   char = 256 ifTrue:[
                       bin := '000000001'
                   ]
                   ifFalse:[
                       char = 0 ifTrue: [
                           bin := '000000000'
                       ]
                       ifFalse:[
                           8 to: 1 by: -1 do:[:index |
                              bin := bin,
                                  ((char bitAt:index) printString).
                           ]
                       ].
                   ].
                   outBytes writeBit: ('0'  asInteger) - shift.
                   1 to: (bin size) by: 1 do: [:index |
                   bit:= ((bin at: index) asInteger) - shift.
                   outBytes writeBit: bit.
                   ].
                ].
            ].
            outBytes writeBit: 1.

            infile reset.
            [infile atEnd not] whileTrue: [
                aChar := infile next asInteger.
                code:= (encodeTable at: aChar). 
                1 to: (code size) by: 1 do: [:index |
                    bit:= ((code at: index) asInteger) - shift.
                    outBytes writeBit: bit.
                ].
           ].
           eof := (encodeTable at: 256).
           1 to: (eof size) by: 1 do: [:index |
                 bit:= ((eof at: index) asInteger) - shift.
                 outBytes writeBit: bit.
           ].     
           outBytes flushByte.
        ]
        ifFalse:[
            stdout << nl << 'binary encode tree' << nl.
            tree postorder: [:char :isLeaf |
                isLeaf ifFalse: [
                    stdout << 1.
                ]
                ifTrue: [
                    bin := ''.
                    char = 256 ifTrue:[
                       bin := '000000001'
                    ]
                    ifFalse:[
                        char = 0 ifTrue: [
                            bin := '000000000'
                        ]
                        ifFalse:[
                            8 to: 1 by: -1 do:[:index |
                            bin := bin,((char bitAt:index) printString).
                           ]
                        ].               
                    ].
                    stdout << 0 << bin.
                ].
            ].
            stdout << 1.

            infile reset.
            stdout << nl << 'input file contents' << nl.
            [infile atEnd not] whileTrue: [
                aChar := infile next asInteger.
                code:= (encodeTable at: aChar).
                stdout << code. 
            ].
            eof := (encodeTable at: 256).
            stdout << eof.
            ].    
        ].
        (opt = $t) ifTrue:[
            encodeTable keysAndValuesDo: [:key :value |
                (value notNil and: [value > 0]) ifTrue: [
                    |index char count string pad|
                    key = 10 ifTrue: [
                        char := 'x0A'.
                    ] 
                    ifFalse: [
                        key = 32 ifTrue: [
                            char := 'x20'.
                        ]
                        ifFalse: [
                           key = 256 ifTrue: [
                               char := 'EOF'.
                           ] 
                            ifFalse: [
                               char := ' ', 
                                   key asCharacter asString, ' '.
                            ].
                        ]
                    ].
                    stdout << char << ' '.
                    count := (freqtable at: key) printString.
                    pad := 7 - count size.
                    [pad > 0] whileTrue: [
                        count := ' ', count.
                        pad := pad - 1.
                    ].
                    stdout << count << '  '.
                    string := value.
                    stdout << string << nl.
                ].
            ].
        ].

    ].

    "Decompression"
    (opt = $u) ifTrue:[
        iseof:=false.
        outBytes := OutBits new: outfile.  
        stack := OrderedCollection new.
        fileStream := BitStream new: infile.
        parseDone := false.
        root := ''.
        node := ''.   
   
        [(fileStream atEnd not) & (iseof not)] whileTrue: [
            curBit := fileStream nextBit.
            curByte := 0.
            parseDone ifFalse:[
                curBit = 0 ifTrue:[
                    8 to:1 by: -1 do:[:i|
                        bit := fileStream nextBit.
                        curByte := curByte bitAt:i put:bit.
                    ].
                    curByte = 0 ifTrue:[
                        noShowBit := fileStream nextBit.
                        noShowBit = 1 ifTrue: [
                            curByte:=curByte bitAt:9 put:1.
                        ].
                    ].
                    stack add: (Leaf new: curByte count: '').
                ].
                curBit = 1 ifTrue:[
                    stack size = 1 ifTrue:[
                        parseDone := true.
                        root := stack removeLast.
                        node := root.
                   ]      
                   ifFalse:[
                       rchild := stack removeLast.
                       lchild := stack removeLast.
                       newTree := (Tree new: -1
                                   count: ''
                                   left: lchild right: rchild).
                      stack add: newTree.
                    ]. 
                ].
            ]
            ifTrue:[
                node isLeaf ifTrue:[
                    ((node char) = 256) ifTrue:[
                        iseof := true.
                    ] 
                    ifFalse:[
                         stdout nextPut: (Character value: (node char)).
                         node := root.
                    ].
                ]
                ifFalse: [
                    curBit=0 ifTrue:[
                        node := node left.
                    ] 
                    ifFalse: [
                        node := node right.
                    ].
                    node isLeaf ifTrue:[
                        iseof not ifTrue:[
                            ((node char)=256) ifTrue:[
                            iseof:=true.
                            ] 
                            ifFalse: [
                                outfile = nil ifTrue:[
                                stdout nextPut: 
                                (Character value: (node char)).
                                ]
                                ifFalse:[
                                    8 to: 1 by: -1 do: [:index |
                                        bit:= (node char) bitAt: index.
                                        outBytes writeBit: bit.
                                    ].
                                ].
                                node := root.
                            ].
                        ].
                    ].
                ].
            ].
        ].
    ].  
].


