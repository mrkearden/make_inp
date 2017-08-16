c234567
      character*60 f1,f2
      character*80 l1
      character*6 dheader,test
      character*1 c, yn
      character*30 name
      c=','
      dheader='    -1'
      write(*,*) ' Name of unv file:'
      read(*,fmt='(a60)') f1
      write(*,*) ' Name of inp file:'
      read(*,fmt='(a60)') f2
      open(unit=1,file=f1,status='old')
      open(unit=2,file=f2,status='unknown')
*
* Find data set 
*
      ln=0
      ifed=0
    1 read(unit=1,fmt='(a80)',end=99) l1 
      ln=ln+1
      test=l1(1:6)
      if (test.eq.dheader) then
       read(1,fmt='(i6)') iset
       write(*,*) 'iset=',iset
*
* node dataset begin
       if (iset.eq.2411) then
        write(*,*) ' **** FOUND NODE DATASET ****'
        write(2,fmt='(a15)') '*node,nset=nall'
    2   read(1,fmt='(a80)') l1
        test=l1(1:6)
        if (test.eq.dheader) then
         write(*,*) 'Number of Nodes Converted:',nl
         goto 98
        endif
        read(l1,fmt='(4I10)') nl,i1,i2,i3
        read(1,fmt='(3E25.16)') fx,fy,fz
        write(2,fmt='(i8,a1,E19.9,a1,E19.9,a1,E19.9)') nl,c,fx,c,fy,c,fz
        goto 2
       endif
* node dataset end
*
* element dataset begin
       if (iset.eq.2412) then
        write(*,*) ' **** FOUND ELEMENT DATASET ****'
 1001   read(1,fmt='(a80)') l1
        test=l1(1:6)
        if (test.eq.dheader) then
         goto 98
        endif
        read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
* Line Elements
  666   if (ifed.eq.22) then
        read(1,fmt='(a80)') l1
         iflag=0
         write(*,*) 'Write 3 noded beams (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1)write(2,fmt='(a28)') 
     &'*element,type=b32,elset=beams'
   21    read(1,fmt='(3i10)') n1,n2,n3
         if (iflag.eq.1) write(2,fmt='(3(i8,a1),i8)') 
     &iel,c,n1,c,n3,c,n2
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.22) goto 666
         read(1,fmt='(a80)') l1
         goto 21
   22   endif
* Line Elements
        if (ifed.eq.11) then
        read(1,fmt='(a80)') l1
         iflag=0
         write(*,*) 'Write 2 noded beams (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1)write(2,fmt='(a28)') 
     &'*element,type=b32,elset=beams'
  321    read(1,fmt='(2i10)') n1,n2
         if (iflag.eq.1) write(2,fmt='(2(i8,a1),i8)') 
     &iel,c,n1,c,n2
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.11) goto 666
         read(1,fmt='(a80)') l1
         goto 321
  322   endif
* s6 Elements
         if (ifed.eq.42) then
         iflag=0
         write(*,*) 'Write 6 node triangles (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1) write(2,fmt='(a29)')
     & '*element,type=s6,elset=shells'
   23    read(1,fmt='(6i10)') n1,n2,n3,n4,n5,n6
         if (iflag.eq.1) write(2,fmt='(6(i8,a1),i8)') 
     &iel,c,n1,c,n3,c,n5,c,n2,c,n4,c,n6
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.42) goto 666
         goto 23
   24   endif
* c3d15 Elements
         if (ifed.eq.113) then
         iflag=0
         write(*,*) 'Write 15 node wedges (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1) write(2,fmt='(a32)')
     & '*element,type=c3d15,elset=wedges'
  823    read(1,fmt='(8i10)') n1,n2,n3,n4,n5,n6,n7,n8
         read(1,fmt='(7i10)') n9,n10,n11,n12,n13,n14,n15
         if (iflag.eq.1) write(2,fmt='(9(i8,a1))') 
     &iel,c,n1,c,n3,c,n5,c,n10,c,n12,c,n14,c,n2,c,n4,c
         if (iflag.eq.1) write(2,fmt='(6(i8,a1),i8)') 
     &n6,c,n11,c,n13,c,n15,c,n7,c,n8,c,n9
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.113) goto 666
         goto 823
  824   endif
* c3d20 Elements
         if (ifed.eq.116) then
         iflag=0
         write(*,*) 'Write 20 node bricks (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1) write(2,fmt='(a32)')
     & '*element,type=c3d20,elset=bricks'
  923    read(1,fmt='(8i10)') n1,n2,n3,n4,n5,n6,n7,n8
         read(1,fmt='(8i10)') n9,n10,n11,n12,n13,n14,n15,n16
         read(1,fmt='(8i10)') n17,n18,n19,n20
         if (iflag.eq.1) write(2,fmt='(9(i8,a1))') 
     &iel,c,n1,c,n3,c,n5,c,n7,c,n13,c,n15,c,n17,c,n19,c
         if (iflag.eq.1) write(2,fmt='(8(i8,a1))') 
     &n2,c,n4,c,n6,c,n8,c,n14,c,n16,c,n18,c,n20,c
         if (iflag.eq.1) write(2,fmt='(3(i8,a1),i8)') 
     &n9,c,n10,c,n11,c,n12
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.116) goto 666
         goto 923
  924   endif
* s3 Elements
         if (ifed.eq.41) then
         iflag=0
         write(*,*) 'Write 3 node triangles (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1) write(2,fmt='(a28)')
     & '*element,type=s3,elset=trias'
  123    read(1,fmt='(3i10)') n1,n2,n3
         if (iflag.eq.1) write(2,fmt='(3(i8,a1),i8)') 
     &iel,c,n1,c,n2,c,n3
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.41) goto 666
         goto 123
  124   endif
* s4 Elements
         if (ifed.eq.44) then
         iflag=0
         write(*,*) 'Write 4 node quads (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1) write(2,fmt='(a28)')
     & '*element,type=s4,elset=quads'
  223    read(1,fmt='(4i10)') n1,n2,n3,n4
         if (iflag.eq.1) write(2,fmt='(4(i8,a1),i8)') 
     &iel,c,n1,c,n2,c,n3,c,n4
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.44) goto 666
         goto 223
  224   endif
* s8 Elements
         if (ifed.eq.45) then
         iflag=0
         write(*,*) 'Write 8 node quads (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1) write(2,fmt='(a28)')
     & '*element,type=s8,elset=quads'
  623    read(1,fmt='(8i10)') n1,n2,n3,n4,n5,n6,n7,n8
         if (iflag.eq.1) write(2,fmt='(8(i8,a1),i8)') 
     &iel,c,n1,c,n3,c,n5,c,n7,c,n2,c,n4,c,n6,c,n8
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.45) goto 666
         goto 623
  624   endif
* Brick Elements
         if (ifed.eq.115) then
         iflag=0
         write(*,*) 'Write 8 node brick (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1) write(2,fmt='(a31)')
     & '*element,type=c3d8,elset=bricks'
  423    read(1,fmt='(8i10)') n1,n2,n3,n4,n5,n6,n7,n8
         if (iflag.eq.1) write(2,fmt='(8(i8,a1),i8)') 
     &iel,c,n1,c,n2,c,n3,c,n4,c,n5,c,n6,c,n7,c,n8
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.115) goto 666
         goto 423
  424   endif
* Wedge Elements
         if (ifed.eq.112) then
         iflag=0
         write(*,*) 'Write 6 node wedge (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1) write(2,fmt='(a31)')
     & '*element,type=c3d6,elset=wedges'
  523    read(1,fmt='(6i10)') n1,n2,n3,n4,n5,n6
         if (iflag.eq.1) write(2,fmt='(6(i8,a1),i8)') 
     &iel,c,n1,c,n2,c,n3,c,n4,c,n5,c,n6
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.112) goto 666
         goto 523
  524   endif
* c3d10 elements
        if (ifed.eq.118) then
        iflag=0
         write(*,*) 'Write 10 node tets (y/n)'
         read(*,*) yn
         if (yn.eq.'y') iflag=1
         if (iflag.eq.1)write(2,fmt='(a32)') 
     &'*element,type=c3d10,elset=solids'
   25    read(1,fmt='(8i10)') n1,n2,n3,n4,n5,n6,n7,n8
         read(1,fmt='(2i10)') n9,n10
         if (iflag.eq.1)write(2,fmt='(9(i8,a1))') 
     &iel,c,n1,c,n3,c,n5,c,n10,c,n2,c,n4
     &,c,n6,c,n7,c
         if (iflag.eq.1)write(2,fmt='(i8,a1,i8)') n8,c,n9
         read(unit=1,fmt='(a80)',end=99) l1
         test=l1(1:6)
         if (test.eq.dheader) goto 98
         read(l1,fmt='(6I10)') iel,ifed,ipid,imid,i4,innodes
         if (ifed.ne.118) goto 666
         goto 25
   28   endif
* Unsupported element read through
       write(*,*) 'Unsupported Element ',ifed
       nrows=0
       frows=real(innodes)/8
       ifrows=int(frows)
       frm=frows-real(ifrows)
       if (frm.gt.0.0) nrows=ifrows+1
       do 1000 i=1,nrows
 1000  read(1,fmt='(8i10)') n1,n2,n3,n4,n5,n6,n7,n8
       goto 1001
* end element processing        
        goto 98
       endif
* element dataset end
      endif
* group processing
      if (iset.eq.2467) then
       write(*,*) '**** Processing Groups ****'
       read(1,fmt='(8i10)') i1,i2,i3,i4,i5,i6,i7,nitems
   45  read(1,fmt='(a30)') name
       write(*,*) 'Group:',name
       write(*,*) ' (1) elset (2) nset (3) Do not write'
       read(*,*) i1
       iflag=0
       if (i1.eq.1) write(2,*) '*elset,elset=',name
       if (i1.eq.2) write(2,*) '*nset,nset=',name
       if (i1.eq.3) iflag=1
       nrows=int((nitems+1)/2)
       write(*,*) 'nrows = ',nrows,nitems
       do 50 i=1,nrows
       read(1,fmt='(8i10)') i1,i2,i3,i4,i5,i6,i7,i8
       if (iflag.eq.0) write(2,fmt='(i10,a1,i10)') i2,c,i6
   50  continue
       read(unit=1,fmt='(a80)') l1
       test=l1(1:6)
       if (test.eq.dheader) goto 99
       read(l1,fmt='(8i10)') i1,i2,i3,i4,i5,i6,i7,nitems
       goto 45
      endif
** read to end of set
    3   read(unit=1,fmt='(a80)',end=99) l1
        test=l1(1:6)
        if (test.ne.dheader) goto 3
** read to end of set
   98 goto 1
   99 end    
