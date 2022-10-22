*************************************************************
* 3 componentes de la funcion f segundo miembro para test
*************************************************************
      function fsmt(x,coef1,coef2)
      implicit double precision(a-h,o-z)
      dimension x(*)
      pi=dacos(-1d0)
*
      fsmt=coef1*fpre(x)-coef2*(1d0-12d0*pi*pi)*dcos(2d0*pi*x(1))*
     &     dcos(2d0*pi*x(2))*dcos(2d0*pi*x(3))
*
      return
      end
*************************************************************
* Funcion para la condicion Dirichlet
*************************************************************
      function fdir(i,x,premas)
      implicit double precision(a-h,o-z)
      dimension x(*)
*
      fdir=fpre(x)
*
      return
      end
*************************************************************
* Funcion g para el test de la condicion Neumann
*************************************************************
      function fg(i,x)
      implicit double precision(a-h,o-z)
      dimension x(*)
      pi=dacos(-1d0)
*
      if (i.eq.1) then
        fg=-2d0*pi*dcos(2d0*pi*x(1))*dsin(2d0*pi*x(2))*
     &     dcos(2d0*pi*x(3))
      else if (i.eq.2) then
        fg=2d0*pi*dsin(2d0*pi*x(1))*dcos(2d0*pi*x(2))*
     &     dcos(2d0*pi*x(3))
      else if (i.eq.3) then
        fg=2d0*pi*dcos(2d0*pi*x(1))*dsin(2d0*pi*x(2))*
     &     dcos(2d0*pi*x(3))
      else if (i.eq.4) then
        fg=-2d0*pi*dsin(2d0*pi*x(1))*dcos(2d0*pi*x(2))*
     &     dcos(2d0*pi*x(3))
      else
        stop 'error en la condicion de contorno Neumann'
      endif
*
      return
      end
*************************************************************
* Funcion phi = Solucion exacta
*************************************************************
      function fpre(x)
      implicit double precision(a-h,o-z)
      dimension x(*)
      pi=dacos(-1d0)
*
      fpre=dcos(2d0*pi*x(1))*dcos(2d0*pi*x(2))*dcos(2d0*pi*x(3))
*
      return
      end
