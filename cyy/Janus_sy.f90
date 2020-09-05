 program main
      implicit none
	  integer n1,n2,n3,n,np,nr0,nc0,s1,f1,m1,a,b,c,e,h,g,r,np1
	  integer,parameter::nlic=3,nbic1=2,nbic2=2  
      integer,parameter::nlic1=3,nbic11=2,nbic22=2
	  real,parameter::frc=0.20
	  real,parameter::ncell=30,ncell1=30,ncell2=30,den=3,pi=3.1415926
	  parameter (n=den*ncell*ncell1*ncell2,np=nlic+nbic1+nbic2,nr0=50,nc0=50)
      parameter(np1=nlic1+nbic11+nbic22)
	  integer,parameter::mnlipid=600,nnlic=nlic*mnlipid,nnbic1=nbic1*mnlipid,nnbic2=nbic2*mnlipid,totnp=nnlic+nnbic1+nnbic2
      integer,parameter::mnlipid1=600
      integer,parameter::nnlic1=nlic1*mnlipid1,nnbic11=nbic11*mnlipid1,nnbic22=nbic22*mnlipid1,totnp1=nnlic1+nnbic11+nnbic22
	  integer,parameter::ns=n-totnp-totnp1
	  real,parameter::dd=0.5
	  real*8 xnn(mnlipid,nr0),ynn(mnlipid,nr0),znn(mnlipid,nr0)
	  real*8 xn(n),yn(n),zn(n),sx(n),sy(n),sz(n)
	  real*8 bxn(mnlipid,nbic1),byn(mnlipid,nbic1),bzn(mnlipid,nbic1)
	  real*8 xc(n),yc(n),zc(n)
	  real*8 xcc(mnlipid,nc0),ycc(mnlipid,nc0),zcc(mnlipid,nc0)
	  real*8 xee(mnlipid,nr0),yee(mnlipid,nr0),zee(mnlipid,nr0)
	  real*8 xe(n),ye(n),ze(n)
      real*8 x11(mnlipid1,nr0),y11(mnlipid1,nr0),z11(mnlipid1,nr0)
      real*8 x22(mnlipid1,nr0),y22(mnlipid1,nr0),z22(mnlipid1,nr0)
      real*8 x33(mnlipid1,nr0),y33(mnlipid1,nr0),z33(mnlipid1,nr0)
      real*8 x1(n),y1(n),z1(n)
      real*8 x2(n),y2(n),z2(n)
      real*8 x3(n),y3(n),z3(n)
	  real e1,e2,v1,v2,xx,yy,zz,rr,dx,dy,dz,a1,a2,a3,a4,a5,a6,a7,b1,b2,b3,y4
      integer x,y
      real z
	  integer i,j,k,l,m,f,s
	  integer bonds,ibonds
	  real*8 sid,box
      integer,parameter::lowx=15,lowy=20,upx=15,upy=20
      real,parameter:: headx1=1.5,heady1=1,zstart1=14.5,zstart11=15.7,zstart2=15.75,zstart22=14.45
      real,parameter:: headx2=1.5,heady2=1,zstart3=19.05,zstart33=11.15,zstart4=17.8,zstart44=12.4
      integer,parameter:: np3=4186
      integer,parameter:: upbond=12,upangle=8
	  !!!set up simulation
	  box=ncell
	  write(*,*)'den(密度)=                         ',den
	  write(*,*)'n(总粒子数)=                       ',n
	  write(*,*)'mnlipid(磷脂分子)=                 ',mnlipid
	  write(*,*)'nlic(每个分子的亲水部分)=          ',nlic
	  write(*,*)'nnlic(所以分子的亲水粒子)=         ',nnlic
	  write(*,*)'nbic1(第一个疏水尾巴)=             ',nbic1
	  write(*,*)'nnbic1(所有的第一条疏水尾巴)=      ',nnbic1
	  write(*,*)'nbic2(第二条疏水尾巴)=             ',nbic2
	  write(*,*)'nnbic2(所有的第二条疏水尾巴)=      ',nnbic2
	  write(*,*)'np(每个磷脂分子的粒子数)=          ',np
	  write(*,*)'totnp(所有磷脂分子总粒子数)=       ',totnp
	  write(*,*)'ns(溶剂粒子数目)=                  ',ns
	  write(*,*)'box(边界条件)=                     ',box
	 
    !!!!!!!!!!!!!!!!!!!!!
    s=0
    y4=5
    do y=1,lowy
    y4=y4+heady1
     b1=5
     do x=1,lowx
      b1=b1+headx1
      s=s+1
      z=zstart1
       do j=1,nlic
          z=z+dd
        xnn(s,j)=b1
        ynn(s,j)=y4
        znn(s,j)=z
        end do
      end do
    end do
    y4=5
    do y=1,lowy
    y4=y4+heady1
     b1=5
     do x=1,lowx
      b1=b1+headx1
      s=s+1
      z=zstart11
       do j=1,nlic
          z=z-dd
        xnn(s,j)=b1
        ynn(s,j)=y4
        znn(s,j)=z
        end do
      end do
    end do
   a=s
     do s1=1,a!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!上半部分的头部链
      do j=1,nlic
       k=(s1-1)*nlic+j
        xn(k)=xnn(s1,j)
        yn(k)=ynn(s1,j)
        zn(k)=znn(s1,j)
        end do
    end do
   f=0
   y4=5
    do y=1,lowy
    y4=y4+heady1
     b1=5
     do x=1,lowx
      b1=b1+headx1
      f=f+1
      a1=b1-0.43
      z=zstart2
       do j=1,nbic1
        z=z+dd
        xcc(f,j)=a1
        ycc(f,j)=y4
        zcc(f,j)=z
        end do
      end do
    end do
    y4=5
    do y=1,lowy
    y4=y4+heady1
     b1=5
     do x=1,lowx
      b1=b1+headx1
      f=f+1
      a1=b1-0.43
      z=zstart22
       do j=1,nbic1
        z=z-dd
        xcc(f,j)=a1
        ycc(f,j)=y4
        zcc(f,j)=z
        end do
      end do
    end do
    b=f!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!上半部尾部链1
    do f1=1,b
      do j=1,nbic1
       k=(f1-1)*nbic1+j
        xc(k)=xcc(f1,j)
        yc(k)=ycc(f1,j)
        zc(k)=zcc(f1,j)
        end do
    end do
   m=0
   y4=5
    do y=1,lowy
    y4=y4+heady1
     b1=5
     do x=1,lowx
     b1=b1+headx1
     a2=b1+0.43
      m=m+1
      z=zstart2
       do j=1,nbic2
        z=z+dd
        xee(m,j)=a2
        yee(m,j)=y4
        zee(m,j)=z
        end do
      end do
    end do
    y4=5
    do y=1,lowy
    y4=y4+heady1
     b1=5
     do x=1,lowx
     b1=b1+headx1
     a2=b1+0.43
      m=m+1
      z=zstart22
       do j=1,nbic2
        z=z-dd
        xee(m,j)=a2
        yee(m,j)=y4
        zee(m,j)=z
        end do
      end do
    end do
    c=m
     do m1=1,c
      do j=1,nbic2
       k=(m1-1)*nbic2+j
        xe(k)=xee(m1,j)
        ye(k)=yee(m1,j)
        ze(k)=zee(m1,j)
        end do
    end do!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!上半部尾部链2
!!!!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%上半部分的磷脂分子排序
   y4=5
    do y=1,upy
    y4=y4+heady2
     b1=5
     do x=1,upx
     b1=b1+headx2
      s=s+1
      z=zstart3
       do j=1,nlic1
        z=z-dd
        x11(s,j)=b1
        y11(s,j)=y4
        z11(s,j)=z
        end do
      end do
    end do
    y4=5
    do y=1,upy
    y4=y4+heady2
     b1=5
     do x=1,upx
     b1=b1+headx2
      s=s+1
      z=zstart33
       do j=1,nlic1
        z=z+dd
        x11(s,j)=b1
        y11(s,j)=y4
        z11(s,j)=z
        end do
      end do
    end do
    do s1=a+1,s!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!上半部分的头部链
      do j=1,nlic1
       k=a+(s1-a-1)*nlic1+j
        x1(k)=x11(s1,j)
        y2(k)=y11(s1,j)
        z3(k)=z11(s1,j)
        end do
    end do!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!下半部分头部链1
    y4=5
    do y=1,upy
    y4=y4+heady2
    b1=5
     do x=1,upx
      b1=b1+headx2
      a3=b1-0.43
      f=f+1
      z=zstart4
       do j=1,nbic11
        z=z-dd
        x22(f,j)=a3
        y22(f,j)=y4
        z22(f,j)=z
        end do
      end do
    end do
     y4=5
    do y=1,upy
    y4=y4+heady2
    b1=5
     do x=1,upx
      b1=b1+headx2
      a3=b1-0.43
      f=f+1
      z=zstart44
       do j=1,nbic11
        z=z+dd
        x22(f,j)=a3
        y22(f,j)=y4
        z22(f,j)=z
        end do
      end do
    end do
     do f1=b+1,f
      do j=1,nbic11
       k=b+(f1-b-1)*nbic11+j
        x2(k)=x22(f1,j)
        y2(k)=y22(f1,j)
        z2(k)=z22(f1,j)
        end do
    end do!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!下半部分尾部链1
    y4=5
      do y=1,upy
      y4=y4+heady2
      b1=5
     do x=1,upx
      b1=b1+headx2
      a4=b1+0.43
      m=m+1
      z=zstart4
       do j=1,nbic22
        z=z-dd
        x33(m,j)=a4
        y33(m,j)=y4
        z33(m,j)=z
        end do
      end do
    end do
    y4=5
      do y=1,upy
      y4=y4+heady2
      b1=5
     do x=1,upx
      b1=b1+headx2
      a4=b1+0.43
      m=m+1
      z=zstart44
       do j=1,nbic22
        z=z+dd
        x33(m,j)=a4
        y33(m,j)=y4
        z33(m,j)=z
        end do
      end do
    end do
     do m1=c+1,m
      do j=1,nbic22
       k=c+(m1-c-1)*nbic22+j
        x3(k)=x33(m1,j)
        y3(k)=y33(m1,j)
        z3(k)=z33(m1,j)
        end do
    end do!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!下半部分尾部链2
    !!!!!!!!!!!!!!!!!!!!!!!!下半部分盗字分子排序
	do i=1,ns                        !!!!!!!!!!!!!!ns代表的是溶剂，在这里就是水分子
  700 call random_number(xx)
      call random_number(yy)
	  call random_number(zz)
	  sx(i)=xx*ncell                      !!!
	  sy(i)=yy*ncell1                      !!!
	  sz(i)=zz*ncell2                      !!!
	    do j=1,i-1                      !!!
		dx=(sx(i)-sx(j))                !!!这部分的作用是为了判断，各个溶剂分子之间是否有重合
		dy=(sy(i)-sy(j))                !!!
		dz=(sz(i)-sz(j))                !!!
		rr=dx**2+dy**2+dz**2            !!!
		if(rr==0) goto 700              !!!
		end do
	  do l=1,a                  !!!!!!!!!!!!!!!!!这部分是为了判断溶剂分子和亲水头部有没有重合，nnlic代表得是所有的亲水粒子
	    dx=(sx(i)-xn(l))            !!!!!
		dy=(sy(i)-yn(l))            !!!!!
		dz=(sz(i)-zn(l))            !!!!!
		rr=dx**2+dy**2+dz**2        !!!!!这一部分的作用是为了判断与亲水链是否重合
		if(rr==0) goto 700          !!!!!
	  end do                        !!!!!
	  do l=1,b                  !!!!!!!!!!!!!!!!!nbic1代表的是第一条疏水尾巴链，nnbic1表示的是第一条疏水链的所有粒子
	    dx=(sx(i)-xc(l))            !!!!
	    dy=(sy(i)-yc(l))            !!!!
	    dz=(sz(i)-zc(l))            !!!!
	    rr=dx**2+dy**2+dz**2        !!!!判断是否和第一条疏水尾巴链重合
	    if(rr==0) goto 700          !!!!
	  end do                        !!!!
	  do l=1,c                !!!!!!!!!!!!!!!!!!nbic2代表的是疏水的第二条链，nnbic2表示的是第二条疏水的所有粒子
	    dx=(sx(i)-xe(l))            !!!!
		dy=(sy(i)-ye(l))            !!!!
		dz=(sz(i)-ze(l))            !!!!
		rr=dx**2+dy**2+dz**2        !!!!这部分表示的是判断溶剂（水）分子是不是和疏水的第二条链的粒子坐标重合
		if(rr==0) goto 700          !!!!
	  end do                        !!!!
     do l=a+1,s-a                  !!!!!!!!!!!!!!!!!这部分是为了判断溶剂分子和亲水头部有没有重合，nnlic代表得是所有的亲水粒子
	    dx=(sx(i)-x1(l))            !!!!!
		dy=(sy(i)-y1(l))            !!!!!
		dz=(sz(i)-z1(l))            !!!!!
		rr=dx**2+dy**2+dz**2        !!!!!这一部分的作用是为了判断与亲水链是否重合
		if(rr==0) goto 700          !!!!!
	  end do                        !!!!!
	  do l=b+1,f-b                  !!!!!!!!!!!!!!!!!nbic1代表的是第一条疏水尾巴链，nnbic1表示的是第一条疏水链的所有粒子
	    dx=(sx(i)-x2(l))            !!!!
	    dy=(sy(i)-y2(l))            !!!!
	    dz=(sz(i)-z2(l))            !!!!
	    rr=dx**2+dy**2+dz**2        !!!!判断是否和第一条疏水尾巴链重合
	    if(rr==0) goto 700          !!!!
	  end do                        !!!!
	  do l=c+1,m-c                 !!!!!!!!!!!!!!!!!!nbic2代表的是疏水的第二条链，nnbic2表示的是第二条疏水的所有粒子
	    dx=(sx(i)-x3(l))            !!!!
		dy=(sy(i)-y3(l))            !!!!
		dz=(sz(i)-z3(l))            !!!!
		rr=dx**2+dy**2+dz**2        !!!!这部分表示的是判断溶剂（水）分子是不是和疏水的第二条链的粒子坐标重合
		if(rr==0) goto 700          !!!!
	  end do                        !!!!
	end do
bonds=mnlipid*(np-1)+mnlipid1*(np1-1)
open(unit=1,file='data.lipid')
write(1,*) 'data file'
write(1,*) ' '
write(1,'(I6,A10)') np*mnlipid+np1*mnlipid1+ns,'atoms'
write(1,'(I6,A10)') bonds,'bonds'
write(1,'(I6,A10)') (np-3)*mnlipid+(np1-3)*mnlipid1,'angles'
write(1,'(I1,A10)') 0,'dihedrals'
write(1,'(I1,A10)') 0,'impropers'
write(1,*) ' '
write(1,'(I1,A11)') 5,'atom types'
write(1,'(I1,A11)') 1,'bond types'
write(1,'(I1,A12)') 3,'angle types'
write(1,'(I1,A15)') 0,'dihedral types'
write(1,'(I1,A15)') 0,'improper types'
write(1,*) ' '
write(1,'(F4.1,F7.1,A5,A4)') 0.0,ncell,'xlo','xhi'
write(1,'(F4.1,F7.1,A5,A4)') 0.0,ncell1,'ylo','yhi'
write(1,'(F4.1,F7.1,A5,A4)') 0.0,ncell2,'zlo','zhi'
write(1,*) ' '
write(1,'(A6)') 'Masses'
write(1,*) ' '
write(1,'(I1,F4.1)') 1,1.0
write(1,'(I1,F4.1)') 2,1.0
write(1,'(I1,F4.1)') 3,1.0
write(1,'(I1,F4.1)') 4,1.0
write(1,'(I1,F4.1)') 5,1.0
write(1,*) ' '
write(1,'(A5)') 'Atoms'
write(1,*)

 do i=1,mnlipid
  do j=1,nlic
  k=(i-1)*np+j
  write(1,'(3I6,3F12.4)') k,i,1,xnn(i,j),ynn(i,j),znn(i,j)
  end do
  do l=1,nbic1
  k=(i-1)*np+nlic+l
  write(1,'(3I6,3F12.4)') k,i,2,xcc(i,l),ycc(i,l),zcc(i,l)
  end do
  do e=1,nbic2
  k=(i-1)*np+nlic+nbic1+e
  write(1,'(3I6,3F12.4)') k,i,2,xee(i,e),yee(i,e),zee(i,e)
  end do!!!!!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%上半部分的粒子坐标输出
end do
a5=k
 do i=mnlipid+1,mnlipid1+mnlipid
  do j=1,nlic1
  k=(i-mnlipid-1)*np1+j+a5
  write(1,'(3I6,3F12.4)') k,i,4,x11(i,j),y11(i,j),z11(i,j)
  end do
  do l=1,nbic11
  k=(i-mnlipid-1)*np1+nlic1+l+a5
  write(1,'(3I6,3F12.4)') k,i,5,x22(i,l),y22(i,l),z22(i,l)
  end do
  do e=1,nbic22
  k=(i-mnlipid-1)*np1+nlic1+nbic11+e+a5
  write(1,'(3I6,3F12.4)') k,i,5,x33(i,e),y33(i,e),z33(i,e)
  end do
end do!!!!!!!!!!!!!!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%下半部分的粒子左边输出
  do l=1,ns
  k=k+1
  write(1,'(3I6,3F12.4)') k,mnlipid1+mnlipid+1,3,sx(l),sy(l),sz(l)
  end do
  write(1,*) ' '
  write(1,'(A5)') 'Bonds'
  write(1,*) ' '
   do i=1,mnlipid
      l=(i-1)*(np-1)+1
   write(1,'(2I6,2I7)') l,1,1+(i-1)*np,2+(i-1)*np
   write(1,'(2I6,2I7)') l+1,1,2+(i-1)*np,3+(i-1)*np
   write(1,'(2I6,2I7)') l+2,1,3+(i-1)*np,4+(i-1)*np
   write(1,'(2I6,2I7)') l+3,1,4+(i-1)*np,5+(i-1)*np
   write(1,'(2I6,2I7)') l+4,1,3+(i-1)*np,6+(i-1)*np
   write(1,'(2I6,2I7)') l+5,1,6+(i-1)*np,7+(i-1)*np
  ! write(1,'(2I6,2I7)') l+6,1,7+(i-1)*np,8+(i-1)*np
  ! write(1,'(2I6,2I7)') l+7,1,8+(i-1)*np,9+(i-1)*np
  ! write(1,'(2I6,2I7)') l+8,1,9+(i-1)*np,10+(i-1)*np
  ! write(1,'(2I6,2I7)') l+9,1,10+(i-1)*np,11+(i-1)*np
  ! write(1,'(2I6,2I7)') l+10,1,11+(i-1)*np,12+(i-1)*np
  ! write(1,'(2I6,2I7)') l+11,1,12+(i-1)*np,13+(i-1)*np
  ! write(1,'(2I6,2I7)') l+12,1,13+(i-1)*np,14+(i-1)*np
  ! write(1,'(2I6,2I7)') l+13,1,14+(i-1)*np,15+(i-1)*np
  ! write(1,'(2I6,2I7)') l+14,1,15+(i-1)*np,16+(i-1)*np
  ! write(1,'(2I6,2I7)') l+15,1,16+(i-1)*np,17+(i-1)*np
  ! write(1,'(2I6,2I7)') l+16,1,17+(i-1)*np,18+(i-1)*np
  ! write(1,'(2I6,2I7)') l+17,1,18+(i-1)*np,19+(i-1)*np
  ! write(1,'(2I6,2I7)') l+18,1,19+(i-1)*np,20+(i-1)*np
  ! write(1,'(2I6,2I7)') l+19,1,10+(i-1)*np,21+(i-1)*np
  ! write(1,'(2I6,2I7)') l+20,1,21+(i-1)*np,22+(i-1)*np
  ! write(1,'(2I6,2I7)') l+21,1,22+(i-1)*np,23+(i-1)*np
  ! write(1,'(2I6,2I7)') l+22,1,23+(i-1)*np,24+(i-1)*np
  ! write(1,'(2I6,2I7)') l+23,1,24+(i-1)*np,25+(i-1)*np
  ! write(1,'(2I6,2I7)') l+24,1,25+(i-1)*np,26+(i-1)*np
  ! write(1,'(2I6,2I7)') l+25,1,26+(i-1)*np,27+(i-1)*np
  ! write(1,'(2I6,2I7)') l+26,1,27+(i-1)*np,28+(i-1)*np
  ! write(1,'(2I6,2I7)') l+27,1,28+(i-1)*np,29+(i-1)*np
  ! write(1,'(2I6,2I7)') l+28,1,29+(i-1)*np,30+(i-1)*np
   end do
   a7=l+5
   do i=mnlipid+1,mnlipid1+mnlipid
      l=(i-mnlipid+1)*(np1-1)+1+a7-upbond
   write(1,'(2I6,2I7)') l,1,1+(i-mnlipid+1)*np1+np3,2+(i-mnlipid+1)*np1+np3
   write(1,'(2I6,2I7)') l+1,1,2+(i-mnlipid+1)*np1+np3,3+(i-mnlipid+1)*np1+np3
   write(1,'(2I6,2I7)') l+2,1,3+(i-mnlipid+1)*np1+np3,4+(i-mnlipid+1)*np1+np3
   write(1,'(2I6,2I7)') l+3,1,4+(i-mnlipid+1)*np1+np3,5+(i-mnlipid+1)*np1+np3
   write(1,'(2I6,2I7)') l+4,1,3+(i-mnlipid+1)*np1+np3,6+(i-mnlipid+1)*np1+np3
   write(1,'(2I6,2I7)') l+5,1,6+(i-mnlipid+1)*np1+np3,7+(i-mnlipid+1)*np1+np3
!   write(1,'(2I6,2I7)') l+6,1,7+(i-mnlipid+1)*np1+np3,8+(i-mnlipid+1)*np1+np3
!   write(1,'(2I6,2I7)') l+7,1,8+(i-mnlipid+1)*np1+np3,9+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+8,1,9+(i-mnlipid+1)*np1+np3,10+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+9,1,10+(i-mnlipid+1)*np1+np3,11+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+10,1,11+(i-mnlipid+1)*np1+np3,12+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+11,1,12+(i-mnlipid+1)*np1+np3,13+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+12,1,13+(i-mnlipid+1)*np1+np3,14+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+13,1,14+(i-mnlipid+1)*np1+np3,15+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+14,1,15+(i-mnlipid+1)*np1+np3,16+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+15,1,16+(i-mnlipid+1)*np1+np3,17+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+16,1,17+(i-mnlipid+1)*np1+np3,18+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+17,1,18+(i-mnlipid+1)*np1+np3,19+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+18,1,19+(i-mnlipid+1)*np1+np3,20+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+19,1,20+(i-mnlipid+1)*np1+np3,21+(i-mnlipid+1)*np1+np3
   !write(1,'(2I6,2I7)') l+20,1,21+(i-mnlipid+1)*np1+np3,22+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+21,1,22+(i-mnlipid+1)*np1+np3,23+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+22,1,23+(i-mnlipid+1)*np1+np3,24+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+23,1,24+(i-mnlipid+1)*np1+np3,25+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+24,1,25+(i-mnlipid+1)*np1+np3,26+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+25,1,26+(i-mnlipid+1)*np1+np3,27+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+26,1,27+(i-mnlipid+1)*np1+np3,28+(i-mnlipid+1)*np1+np3
  ! write(1,'(2I6,2I7)') l+27,1,28+(i-mnlipid+1)*np1+np3,29+(i-mnlipid+1)*np1+np3
 !  write(1,'(2I6,2I7)') l+28,1,29+(i-1)*np,30+(i-1)*np
   end do
write(1,*) ' '
write(1,'(A6)') 'Angles'
write(1,*) ' '
  do i=1,mnlipid
  l=(i-1)*(np-3)+1
  write(1,'(2I6,3I7)') l,1,1+(i-1)*np,2+(i-1)*np,3+(i-1)*np
 ! write(1,'(2I6,3I7)') l+1,1,2+(i-1)*np,3+(i-1)*np,4+(i-1)*np
 ! write(1,'(2I6,3I7)') l+2,1,3+(i-1)*np,4+(i-1)*np,5+(i-1)*np
 ! write(1,'(2I6,3I7)') l+3,1,4+(i-1)*np,5+(i-1)*np,6+(i-1)*np
 ! write(1,'(2I6,3I7)') l+4,1,5+(i-1)*np,6+(i-1)*np,7+(i-1)*np
 ! write(1,'(2I6,3I7)') l+5,1,6+(i-1)*np,7+(i-1)*np,8+(i-1)*np
 ! write(1,'(2I6,3I7)') l+6,1,7+(i-1)*np,8+(i-1)*np,9+(i-1)*np
 ! write(1,'(2I6,3I7)') l+7,1,8+(i-1)*np,9+(i-1)*np,10+(i-1)*np
!  write(1,'(2I6,3I7)') l+7,1,10+(i-1)*np,11+(i-1)*np,12+(i-1)*np
 ! write(1,'(2I6,3I7)') l+8,1,11+(i-1)*np,12+(i-1)*np,13+(i-1)*np
 ! write(1,'(2I6,3I7)') l+9,1,12+(i-1)*np,13+(i-1)*np,14+(i-1)*np
 ! write(1,'(2I6,3I7)') l+10,1,13+(i-1)*np,14+(i-1)*np,15+(i-1)*np
 ! write(1,'(2I6,3I7)') l+11,1,14+(i-1)*np,15+(i-1)*np,16+(i-1)*np
 ! write(1,'(2I6,3I7)') l+12,1,15+(i-1)*np,16+(i-1)*np,17+(i-1)*np
 ! write(1,'(2I6,3I7)') l+13,1,16+(i-1)*np,17+(i-1)*np,18+(i-1)*np
 ! write(1,'(2I6,3I7)') l+14,1,17+(i-1)*np,18+(i-1)*np,19+(i-1)*np
 ! write(1,'(2I6,3I7)') l+15,1,18+(i-1)*np,19+(i-1)*np,20+(i-1)*np
 ! write(1,'(2I6,3I7)') l+15,1,20+(i-1)*np,21+(i-1)*np,22+(i-1)*np
 ! write(1,'(2I6,3I7)') l+16,1,21+(i-1)*np,22+(i-1)*np,23+(i-1)*np
 ! write(1,'(2I6,3I7)') l+17,1,22+(i-1)*np,23+(i-1)*np,24+(i-1)*np
 ! write(1,'(2I6,3I7)') l+18,1,23+(i-1)*np,24+(i-1)*np,25+(i-1)*np
 ! write(1,'(2I6,3I7)') l+19,1,24+(i-1)*np,25+(i-1)*np,26+(i-1)*np
 ! write(1,'(2I6,3I7)') l+20,1,25+(i-1)*np,26+(i-1)*np,27+(i-1)*np
 ! write(1,'(2I6,3I7)') l+21,1,26+(i-1)*np,27+(i-1)*np,28+(i-1)*np
 ! write(1,'(2I6,3I7)') l+22,1,27+(i-1)*np,28+(i-1)*np,29+(i-1)*np
 ! write(1,'(2I6,3I7)') l+24,1,28+(i-1)*np,29+(i-1)*np,30+(i-1)*np
  write(1,'(2I6,3I7)') l+1,2,2+(i-1)*np,3+(i-1)*np,4+(i-1)*np
  write(1,'(2I6,3I7)') l+2,2,2+(i-1)*np,3+(i-1)*np,6+(i-1)*np
  write(1,'(2I6,3I7)') l+3,3,4+(i-1)*np,3+(i-1)*np,6+(i-1)*np
  end do
  a6=l+3
  do i=mnlipid+1,mnlipid1+mnlipid
  l=(i-mnlipid+1)*(np1-3)+1+a6-upangle
  write(1,'(2I6,3I7)') l,1,1+(i-mnlipid+1)*np1+np3,2+(i-mnlipid+1)*np1+np3,3+(i-mnlipid+1)*np1+np3
!  write(1,'(2I6,3I7)') l+1,1,2+(i-mnlipid+1)*np1+np3,3+(i-mnlipid+1)*np1+np3,4+(i-mnlipid+1)*np1+np3
!  write(1,'(2I6,3I7)') l+2,1,3+(i-mnlipid+1)*np1+np3,4+(i-mnlipid+1)*np1+np3,5+(i-mnlipid+1)*np1+np3
!  write(1,'(2I6,3I7)') l+1,1,4+(i-mnlipid+1)*np1+np3,5+(i-mnlipid+1)*np1+np3,6+(i-mnlipid+1)*np1+np3
!  write(1,'(2I6,3I7)') l+2,1,7+(i-mnlipid+1)*np1+np3,8+(i-mnlipid+1)*np1+np3,9+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+5,1,10+(i-mnlipid+1)*np1+np3,11+(i-mnlipid+1)*np1+np3,12+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+6,1,11+(i-mnlipid+1)*np1+np3,12+(i-mnlipid+1)*np1+np3,13+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+7,1,8+(i-1)*np1,9+(i-1)*np1,10+(i-1)*np1
 ! write(1,'(2I6,3I7)') l+7,1,12+(i-mnlipid+1)*np1+np3,13+(i-mnlipid+1)*np1+np3,14+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+8,1,13+(i-mnlipid+1)*np1+np3,14+(i-mnlipid+1)*np1+np3,15+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+9,1,14+(i-mnlipid+1)*np1+np3,15+(i-mnlipid+1)*np1+np3,16+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+10,1,15+(i-mnlipid+1)*np1+np3,16+(i-mnlipid+1)*np1+np3,17+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+11,1,16+(i-mnlipid+1)*np1+np3,17+(i-mnlipid+1)*np1+np3,18+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+12,1,17+(i-mnlipid+1)*np1+np3,18+(i-mnlipid+1)*np1+np3,19+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+13,1,18+(i-mnlipid+1)*np1+np3,19+(i-mnlipid+1)*np1+np3,20+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+14,1,19+(i-mnlipid+1)*np1+np3,20+(i-mnlipid+1)*np1+np3,21+(i-mnlipid+1)*np1+np3
!  write(1,'(2I6,3I7)') l+15,1,18+(i-1)*np1,19+(i-1)*np1,20+(i-1)*np1
 ! write(1,'(2I6,3I7)') l+15,1,20+(i-mnlipid+1)*np1+np3,21+(i-mnlipid+1)*np1+np3,22+(i-mnlipid+1)*np1+np3
!  write(1,'(2I6,3I7)') l+16,1,21+(i-mnlipid+1)*np1+np3,22+(i-mnlipid+1)*np1+np3,23+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+17,1,22+(i-mnlipid+1)*np1+np3,23+(i-mnlipid+1)*np1+np3,24+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+18,1,23+(i-mnlipid+1)*np1+np3,24+(i-mnlipid+1)*np1+np3,25+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+19,1,24+(i-mnlipid+1)*np1+np3,25+(i-mnlipid+1)*np1+np3,26+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+20,1,25+(i-mnlipid+1)*np1+np3,26+(i-mnlipid+1)*np1+np3,27+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+21,1,26+(i-mnlipid+1)*np1+np3,27+(i-mnlipid+1)*np1+np3,28+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+22,1,27+(i-mnlipid+1)*np1+np3,28+(i-mnlipid+1)*np1+np3,29+(i-mnlipid+1)*np1+np3
 ! write(1,'(2I6,3I7)') l+23,1,28+(i-1)*np,29+(i-1)*np,30+(i-1)*np1
  write(1,'(2I6,3I7)') l+1,2,2+(i-mnlipid+1)*np1+np3,3+(i-mnlipid+1)*np1+np3,4+(i-mnlipid+1)*np1+np3
  write(1,'(2I6,3I7)') l+2,2,2+(i-mnlipid+1)*np1+np3,3+(i-mnlipid+1)*np1+np3,6+(i-mnlipid+1)*np1+np3
  write(1,'(2I6,3I7)') l+3,3,4+(i-mnlipid+1)*np1+np3,3+(i-mnlipid+1)*np1+np3,6+(i-mnlipid+1)*np1+np3
  end do
end program