 program main
      implicit none
	  integer n1,n2,n3,n,np,nr0,nc0,s1,f1,m1,a,b,c,e,h,g,r,np1
	  integer,parameter::nlic=3,nbic1=3,nbic2=3  
      integer,parameter::nlic1=2,nbic11=2
	  real,parameter::frc=0.20
	  real,parameter::ncell=30,ncell1=30,ncell2=30,den=3,pi=3.1415926
	  parameter (n=den*ncell*ncell1*ncell2,np=nlic+nbic1+nbic2,nr0=50,nc0=50)
      parameter(np1=nlic1+nbic11)
	  integer,parameter::mnlipid=600,nnlic=nlic*mnlipid,nnbic1=nbic1*mnlipid,nnbic2=nbic2*mnlipid,totnp=nnlic+nnbic1+nnbic2
      integer,parameter::mnlipid1=380
      integer,parameter::nnlic1=nlic1*mnlipid1,nnbic11=nbic11*mnlipid1,totnp1=nnlic1+nnbic11
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
	  !!!set up simulation
	  box=ncell
	  write(*,*)'den(�ܶ�)=                         ',den
	  write(*,*)'n(��������)=                       ',n
	  write(*,*)'mnlipid(��֬����)=                 ',mnlipid
	  write(*,*)'nlic(ÿ�����ӵ���ˮ����)=          ',nlic
	  write(*,*)'nnlic(���Է��ӵ���ˮ����)=         ',nnlic
	  write(*,*)'nbic1(��һ����ˮβ��)=             ',nbic1
	  write(*,*)'nnbic1(���еĵ�һ����ˮβ��)=      ',nnbic1
	  write(*,*)'nbic2(�ڶ�����ˮβ��)=             ',nbic2
	  write(*,*)'nnbic2(���еĵڶ�����ˮβ��)=      ',nnbic2
	  write(*,*)'np(ÿ����֬���ӵ�������)=          ',np
	  write(*,*)'totnp(������֬������������)=       ',totnp
	  write(*,*)'ns(�ܼ�������Ŀ)=                  ',ns
	  write(*,*)'box(�߽�����)=                     ',box
   
    ! 红上
    s=0
    y4=5
    do y=1,20
    y4=y4+1
     b1=5
     do x=1,15
      b1=b1+1.33
      s=s+1
      z=14.5
       do j=1,nlic
          z=z+dd
        xnn(s,j)=b1
        ynn(s,j)=y4
        znn(s,j)=z
        end do
      end do
    end do

    ! 红下
    y4=5
    do y=1,20
    y4=y4+1
     b1=5
     do x=1,15
      b1=b1+1.33
      s=s+1
      z=15.7
       do j=1,nlic
          z=z-dd
        xnn(s,j)=b1
        ynn(s,j)=y4
        znn(s,j)=z
        end do
      end do
    end do

   a=s
     do s1=1,a!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!�ϰ벿�ֵ�ͷ����
      do j=1,nlic
       k=(s1-1)*nlic+j
        xn(k)=xnn(s1,j)
        yn(k)=ynn(s1,j)
        zn(k)=znn(s1,j)
        end do
    end do


  ! 蓝LU
   f=0
   y4=5
    do y=1,20
    y4=y4+1
     b1=5
     do x=1,15
      b1=b1+1.33
      f=f+1
      a1=b1-0.43
      z=15.75
       do j=1,nbic1
        z=z+dd
        xcc(f,j)=a1
        ycc(f,j)=y4
        zcc(f,j)=z
        end do
      end do
    end do
    ! 蓝LD
    y4=5
    do y=1,20
    y4=y4+1
     b1=5
     do x=1,15
      b1=b1+1.33
      f=f+1
      a1=b1-0.43
      z=14.45
       do j=1,nbic1
        z=z-dd
        xcc(f,j)=a1
        ycc(f,j)=y4
        zcc(f,j)=z
        end do
      end do
    end do
    b=f!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!�ϰ벿β����1
    do f1=1,b
      do j=1,nbic1
       k=(f1-1)*nbic1+j
        xc(k)=xcc(f1,j)
        yc(k)=ycc(f1,j)
        zc(k)=zcc(f1,j)
        end do
    end do

  ! 蓝RU
   m=0
   y4=5
    do y=1,20
    y4=y4+1
     b1=5
     do x=1,15
     b1=b1+1.33
     a2=b1+0.43
      m=m+1
      z=15.75
       do j=1,nbic2
        z=z+dd
        xee(m,j)=a2
        yee(m,j)=y4
        zee(m,j)=z
        end do
      end do
    end do

    ! 蓝RD
    y4=5
    do y=1,20
    y4=y4+1
     b1=5
     do x=1,15
     b1=b1+1.33
     a2=b1+0.43
      m=m+1
      z=14.45
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
    end do!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!�ϰ벿β����2
!!!!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%�ϰ벿�ֵ���֬��������
  
  ! 粉U
   y4=5
    do y=1,10
    y4=y4+2
     b1=5
     do x=1,19
     b1=b1+1.05
      s=s+1
      z=19.3
       do j=1,nlic1
        z=z-dd
        x11(s,j)=b1
        y11(s,j)=y4
        z11(s,j)=z
        end do
      end do
    end do

    ! 粉D
    y4=5
    do y=1,10
    y4=y4+2
     b1=5
     do x=1,19
     b1=b1+1.05
      s=s+1
      z=10.9
       do j=1,nlic1
        z=z+dd
        x11(s,j)=b1
        y11(s,j)=y4
        z11(s,j)=z
        end do
      end do
    end do
    do s1=a+1,s!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!�ϰ벿�ֵ�ͷ����
      do j=1,nlic1
       k=a+(s1-a-1)*nlic1+j
        x1(k)=x11(s1,j)
        y2(k)=y11(s1,j)
        z3(k)=z11(s1,j)
        end do
    end do!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!�°벿��ͷ����1

    ! 绿上
    y4=5
    do y=1,10
    y4=y4+2
    b1=5
     do x=1,19
      b1=b1+1.05
!      a3=b1-0.43
      f=f+1
      z=18.3
       do j=1,nbic11
        z=z-dd
        x22(f,j)=b1
        y22(f,j)=y4
        z22(f,j)=z
        end do
      end do
    end do
    
    ! 绿下 
     y4=5
    do y=1,10
    y4=y4+2
    b1=5
     do x=1,19
      b1=b1+1.05
!      a3=b1-0.43
      f=f+1
      z=11.9
       do j=1,nbic11
        z=z+dd
        x22(f,j)=b1
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
    end do!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!�°벿��β����1



	do i=1,ns                        !!!!!!!!!!!!!!ns���������ܼ������������ˮ����
  700 call random_number(xx)
      call random_number(yy)
	  call random_number(zz)
	  sx(i)=xx*ncell                      !!!
	  sy(i)=yy*ncell1                      !!!
	  sz(i)=zz*ncell2                      !!!
	    do j=1,i-1                      !!!
		dx=(sx(i)-sx(j))                !!!�ⲿ�ֵ�������Ϊ���жϣ������ܼ�����֮���Ƿ����غ�
		dy=(sy(i)-sy(j))                !!!
		dz=(sz(i)-sz(j))                !!!
		rr=dx**2+dy**2+dz**2            !!!
		if(rr==0) goto 700              !!!
		end do
	  do l=1,a                  !!!!!!!!!!!!!!!!!�ⲿ����Ϊ���ж��ܼ����Ӻ���ˮͷ����û���غϣ�nnlic�����������е���ˮ����
	    dx=(sx(i)-xn(l))            !!!!!
		dy=(sy(i)-yn(l))            !!!!!
		dz=(sz(i)-zn(l))            !!!!!
		rr=dx**2+dy**2+dz**2        !!!!!��һ���ֵ�������Ϊ���ж�����ˮ���Ƿ��غ�
		if(rr==0) goto 700          !!!!!
	  end do                        !!!!!
	  do l=1,b                  !!!!!!!!!!!!!!!!!nbic1�������ǵ�һ����ˮβ������nnbic1��ʾ���ǵ�һ����ˮ������������
	    dx=(sx(i)-xc(l))            !!!!
	    dy=(sy(i)-yc(l))            !!!!
	    dz=(sz(i)-zc(l))            !!!!
	    rr=dx**2+dy**2+dz**2        !!!!�ж��Ƿ�͵�һ����ˮβ�����غ�
	    if(rr==0) goto 700          !!!!
	  end do                        !!!!
	  do l=1,c                !!!!!!!!!!!!!!!!!!nbic2����������ˮ�ĵڶ�������nnbic2��ʾ���ǵڶ�����ˮ����������
	    dx=(sx(i)-xe(l))            !!!!
		dy=(sy(i)-ye(l))            !!!!
		dz=(sz(i)-ze(l))            !!!!
		rr=dx**2+dy**2+dz**2        !!!!�ⲿ�ֱ�ʾ�����ж��ܼ���ˮ�������ǲ��Ǻ���ˮ�ĵڶ����������������غ�
		if(rr==0) goto 700          !!!!
	  end do                        !!!!
     do l=a+1,s-a                  !!!!!!!!!!!!!!!!!�ⲿ����Ϊ���ж��ܼ����Ӻ���ˮͷ����û���غϣ�nnlic�����������е���ˮ����
	    dx=(sx(i)-x1(l))            !!!!!
		dy=(sy(i)-y1(l))            !!!!!
		dz=(sz(i)-z1(l))            !!!!!
		rr=dx**2+dy**2+dz**2        !!!!!��һ���ֵ�������Ϊ���ж�����ˮ���Ƿ��غ�
		if(rr==0) goto 700          !!!!!
	  end do                        !!!!!
	  do l=b+1,f-b                  !!!!!!!!!!!!!!!!!nbic1�������ǵ�һ����ˮβ������nnbic1��ʾ���ǵ�һ����ˮ������������
	    dx=(sx(i)-x2(l))            !!!!
	    dy=(sy(i)-y2(l))            !!!!
	    dz=(sz(i)-z2(l))            !!!!
	    rr=dx**2+dy**2+dz**2        !!!!�ж��Ƿ�͵�һ����ˮβ�����غ�
	    if(rr==0) goto 700          !!!!
	  end do                        !!!!
	  do l=c+1,m-c                 !!!!!!!!!!!!!!!!!!nbic2����������ˮ�ĵڶ�������nnbic2��ʾ���ǵڶ�����ˮ����������
	    dx=(sx(i)-x3(l))            !!!!
		dy=(sy(i)-y3(l))            !!!!
		dz=(sz(i)-z3(l))            !!!!
		rr=dx**2+dy**2+dz**2        !!!!�ⲿ�ֱ�ʾ�����ж��ܼ���ˮ�������ǲ��Ǻ���ˮ�ĵڶ����������������غ�
		if(rr==0) goto 700          !!!!
	  end do                        !!!!
	end do
bonds=mnlipid*(np-1)+mnlipid1*(np1-1)
open(unit=1,file='data.lipid')
write(1,*) 'data file'
write(1,*) ' '
write(1,'(I6,A10)') np*mnlipid+np1*mnlipid1+ns,'atoms'
write(1,'(I6,A10)') bonds,'bonds'
write(1,'(I6,A10)') (np-3)*mnlipid+(np1-2)*mnlipid1,'angles'
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
  end do!!!!!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%�ϰ벿�ֵ������������
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
end do!!!!!!!!!!!!!!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%�°벿�ֵ�����������
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
   write(1,'(2I6,2I7)') l+4,1,5+(i-1)*np,6+(i-1)*np
   write(1,'(2I6,2I7)') l+5,1,3+(i-1)*np,7+(i-1)*np
   write(1,'(2I6,2I7)') l+6,1,7+(i-1)*np,8+(i-1)*np
   write(1,'(2I6,2I7)') l+7,1,8+(i-1)*np,9+(i-1)*np
   end do
   a7=l+7
   do i=mnlipid+1,mnlipid1+mnlipid
      l=(i-mnlipid+1)*(np1-1)+1+a7-6
   write(1,'(2I6,2I7)') l,1,1+(i-mnlipid+1)*np1+5392,2+(i-mnlipid+1)*np1+5392
   write(1,'(2I6,2I7)') l+1,1,2+(i-mnlipid+1)*np1+5392,3+(i-mnlipid+1)*np1+5392
   write(1,'(2I6,2I7)') l+2,1,3+(i-mnlipid+1)*np1+5392,4+(i-mnlipid+1)*np1+5392
   end do
write(1,*) ' '
write(1,'(A6)') 'Angles'
write(1,*) ' '
  do i=1,mnlipid
  l=(i-1)*(np-3)+1
  write(1,'(2I6,3I7)') l,1,1+(i-1)*np,2+(i-1)*np,3+(i-1)*np
  write(1,'(2I6,3I7)') l+1,1,4+(i-1)*np,5+(i-1)*np,6+(i-1)*np
  write(1,'(2I6,3I7)') l+2,1,7+(i-1)*np,8+(i-1)*np,9+(i-1)*np
  write(1,'(2I6,3I7)') l+3,2,2+(i-1)*np,3+(i-1)*np,4+(i-1)*np
  write(1,'(2I6,3I7)') l+4,2,2+(i-1)*np,3+(i-1)*np,7+(i-1)*np
  write(1,'(2I6,3I7)') l+5,3,4+(i-1)*np,3+(i-1)*np,7+(i-1)*np
  end do
  a6=l+5
  do i=mnlipid+1,mnlipid1+mnlipid
  l=(i-mnlipid+1)*(np1-3)+1+a6-2
  write(1,'(2I6,3I7)') l,1,1+(i-mnlipid+1)*np1+5392,2+(i-mnlipid+1)*np1+5392,3+(i-mnlipid+1)*np1+5392
  write(1,'(2I6,3I7)') l+1,1,2+(i-mnlipid+1)*np1+5392,3+(i-mnlipid+1)*np1+5392,4+(i-mnlipid+1)*np1+5392
  end do
end program