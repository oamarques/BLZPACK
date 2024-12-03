######################################################################
#                                                                    #
# This Makefile generates libblzpack.a. Instructions and options for #
# the compiler and the command for generating the library are given  #
# in ../make.inc.                                                    # 
#                                                                    #
######################################################################

###################
include ../make.inc
###################

all: ../lib/$(BLZPACKLIB)

double = dblzdrv.f dblzeig.f dblzend.f dblzext.f dblzfct.f dblzkll.f \
	 dblzrbx.f dblzset.f dblzstp.f dblzstr.f ddatchk.f ddatset.f \
	 deigchk.f deigidx.f deigprt.f deigsrt.f didenty.f dlzchek.f \
	 dlzcopy.f dlzerrs.f dlzhead.f dlzhist.f dlzioop.f dlzmmry.f \
	 dlznrmx.f dlzpntr.f dlzprt1.f dlzprt2.f dlzprt3.f dlzprt4.f \
	 dlzprt5.f dlzprt6.f dlzprt7.f dlzprt8.f dlzrang.f dlzstp1.f \
	 dlzstp2.f dlzstp3.f dlzstp4.f dlzstp5.f dlzstp6.f dlzstts.f \
     dlztime.f dlzwrns.f dmgschm.f dneigab.f dnorm2a.f dporth.f  \
	 dpurge.f  dpythag.f dqtbq.f   dqtbr.f   drandnr.f dreorth.f \
	 dresnrm.f drfactr.f drqalph.f drqbeta.f drvcomp.f drvmngr.f \
	 drvrstr.f dsetdft.f dseteps.f dsetlrm.f dsetssl.f dsetto0.f \
	 dsorth.f  dsorth0.f dssback.f dssbext.f dssbnds.f dsschek.f \
	 dssclsd.f dsshist.f dssichk.f dssmovb.f dssneed.f dssorgn.f \
	 dssrang.f dssrstr.f dsssigl.f dsssigr.f dssslct.f dssspec.f \
	 dsssplt.f dssstat.f dsstrsf.f dstartr.f dstartx.f dtbalph.f \
	 dtbbeta.f dtbbred.f dtbconv.f dtbeigp.f dtbillc.f dtbimql.f \
	 dupbeta.f dupeta.f  duptau.f  drvstor.f d$(VERSION).f \
	 dtbcopy_$(TRDEIG).f dtbtrid_$(TRDEIG).f d$(STIMER).f \
	 d$(SRANDN).f

single = sblzdrv.f sblzeig.f sblzend.f sblzext.f sblzfct.f sblzkll.f \
	 sblzrbx.f sblzset.f sblzstp.f sblzstr.f sdatchk.f sdatset.f \
	 seigchk.f seigidx.f seigprt.f seigsrt.f sidenty.f slzchek.f \
	 slzcopy.f slzerrs.f slzhead.f slzhist.f slzioop.f slzmmry.f \
	 slznrmx.f slzpntr.f slzprt1.f slzprt2.f slzprt3.f slzprt4.f \
	 slzprt5.f slzprt6.f slzprt7.f slzprt8.f slzrang.f slzstp1.f \
	 slzstp2.f slzstp3.f slzstp4.f slzstp5.f slzstp6.f slzstts.f \
	 slztime.f slzwrns.f smgschm.f sneigab.f snorm2a.f sporth.f  \
	 spurge.f  spythag.f sqtbq.f   sqtbr.f   srandnr.f sreorth.f \
	 sresnrm.f srfactr.f srqalph.f srqbeta.f srvcomp.f srvmngr.f \
	 srvrstr.f ssetdft.f sseteps.f ssetlrm.f ssetssl.f ssetto0.f \
	 ssorth0.f ssorth.f  sssback.f sssbext.f sssbnds.f ssschek.f \
	 sssclsd.f ssshist.f sssichk.f sssmovb.f sssneed.f sssorgn.f \
	 sssrang.f sssrstr.f ssssigl.f ssssigr.f sssslct.f sssspec.f \
	 ssssplt.f sssstat.f ssstrsf.f sstartr.f sstartx.f stbalph.f \
	 stbbeta.f stbbred.f stbconv.f stbeigp.f stbillc.f stbimql.f \
	 supbeta.f supeta.f  suptau.f  srvstor.f s$(VERSION).f \
	 stbcopy_$(TRDEIG).f stbtrid_$(TRDEIG).f s$(STIMER).f \
	 s$(SRANDN).f

integr = irvstor.f i$(VERSION).f 

auxsrc = bittest.f blasdef.f 

allsrc = $(double) $(single) $(integr) $(auxsrc)
	      
OBJECT = $(allsrc:.f=.o)

../lib/$(BLZPACKLIB): $(OBJECT)
	$(ARCH) $(ARCHFLAGS) $@ $(OBJECT) 
	$(RANLIB) $@

dseteps.o: 
	$(FC) $(FCNOOPT) -c -o $@ dseteps.f

sseteps.o:
	$(FC) $(FCNOOPT) -c -o $@ sseteps.f

.f.o:
	$(FC) $(FCOPT) $(FCFIXDF) -c -o $@ $<

clean:
	rm -f *.o

# end of Makefile ######################################################
