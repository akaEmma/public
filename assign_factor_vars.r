assign_factor_vars <- function(df) {
    df <- df %>% mutate(AGEGRP = factor(AGEGRP, 
                                              levels = AGEGRPlevels, 
                                              labels = AGEGRPlabels))
    df <- df %>% mutate(LANGUAGE = factor(LANGUAGE, 
                                                levels = LANGUAGElevels,
                                                labels = LANGUAGElabels))
    df <- df %>% mutate(HAD_CPOX = factor(HAD_CPOX, 
                                                levels = YNDKRFlevels, 
                                                labels = YNDKRFlabels))
    df <- df %>% mutate(CWIC_02 = factor(CWIC_02, levels = YNDKRFlevels, labels = YNDKRFlabels))
    df <- df %>% mutate(CBF_01 = factor(CBF_01, levels = YNDKRFlevels, labels = YNDKRFlabels))
    
    df <- df %>% mutate(P_NUHEPX = factor(P_NUHEPX, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(U3D_HEP = factor(U3D_HEP, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(U2D_HEP = factor(U2D_HEP, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(U1D_HEP = factor(U1D_HEP, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_UTDHEPA1 = factor(P_UTDHEPA1, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUMVRX = factor(P_NUMVRX, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMVRN = factor(P_NUMVRN, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMVRC = factor(P_NUMVRC, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMTPN = factor(P_NUMTPN, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMROT = factor(P_NUMROT, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUMRO = factor(P_NUMRO, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMRM = factor(P_NUMRM, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMRG = factor(P_NUMRG, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMRB = factor(P_NUMRB, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMPOL = factor(P_NUMPOL, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUMPCN = factor(P_NUMPCN, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMPCCN = factor(P_NUMPCCN, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMPCC13 = factor(P_NUMPCC13, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMPCC7 = factor(P_NUMPCC7, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMPCC = factor(P_NUMPCC, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUMPCP = factor(P_NUMPCP, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMPCV = factor(P_NUMPCV, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMOPV = factor(P_NUMOPV, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMOLN = factor(P_NUMOLN, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMMSR = factor(P_NUMMSR, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUMMSM = factor(P_NUMMSM, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMMS = factor(P_NUMMS, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMMRV = factor(P_NUMMRV, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMMPR = factor(P_NUMMPR, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMMP = factor(P_NUMMP, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUMMMX = factor(P_NUMMMX, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMMMRX = factor(P_NUMMMRX, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMMMR = factor(P_NUMMMR, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMMCN = factor(P_NUMMCN, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMIPV = factor(P_NUMIPV, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUMHS = factor(P_NUMHS, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMHM = factor(P_NUMHM, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMHION = factor(P_NUMHION, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMHIN = factor(P_NUMHIN, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMHIB = factor(P_NUMHIB, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUMHHY = factor(P_NUMHHY, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMHG = factor(P_NUMHG, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMHEP = factor(P_NUMHEP, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMHEN = factor(P_NUMHEN, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMHEA = factor(P_NUMHEA, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUMFLUN = factor(P_NUMFLUN, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMFLUM = factor(P_NUMFLUM, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMFLUL = factor(P_NUMFLUL, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMFLU = factor(P_NUMFLU, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMDTP = factor(P_NUMDTP, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUMDTA = factor(P_NUMDTA, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMDIH = factor(P_NUMDIH, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMDHI = factor(P_NUMDHI, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUMDAH = factor(P_NUMDAH, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(P_NUHPHB = factor(P_NUHPHB, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    
    df <- df %>% mutate(P_NUHIBX = factor(P_NUHIBX, levels = SHOTCOUNlevels, labels = SHOTCOUNlabels))
    df <- df %>% mutate(D7 = factor(D7, levels = YNlevels, labels = YNlabels))
    df <- df %>% mutate(BFENDFL06 = factor(BFENDFL06, levels = Ylevels, labels = Ylabels))
    df <- df %>% mutate(BFFORMFL06 = factor(BFFORMFL06, levels = Ylevels, labels = Ylabels))
    df <- df %>% mutate(CHILDNM = factor(CHILDNM, levels = CHILDNMlevels, labels = CHILDNMlabels))
    
    df <- df %>% mutate(CWIC_01 = factor(CWIC_01, levels = CWIClevels, labels = CWIClabels))
    df <- df %>% mutate(EDUC1 = factor(EDUC1, levels = EDUC1_levels, labels = EDUC1_labels))
    df <- df %>% mutate(I_HISP_K = factor(I_HISP_K, levels = HISPlevels, labels = HISPlabels))
    df <- df %>% mutate(MOBIL_I = factor(MOBIL_I, levels = MOBILlevels, labels = MOBILlabels))
    df <- df %>% mutate(SEX = factor(SEX, levels = SEXlevels, labels = SEXlabels))
    
    df <- df %>% mutate(INCPOV1 = factor(INCPOV1, levels = INCPOVlevels, labels = INCPOVlabels))
    df <- df %>% mutate(PDAT = factor(PDAT, levels = HASPDA2Flevels, labels = HASPDA2Flabels))
    df <- df %>% mutate(PROV_FAC = factor(PROV_FAC, levels = PROVIDlevels, labels = PROVIDlabels))
    df <- df %>% mutate(REGISTRY = factor(REGISTRY, levels = REGISTRYlevels, labels = REGISTRYlabels))
    df <- df %>% mutate(VFC_ORDER = factor(VFC_ORDER, levels = REGISTRYlevels, labels = REGISTRYlabels))
    
    df <- df %>% mutate(XDTPTY1 = factor(XDTPTY1, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XVRCTY9 = factor(XVRCTY9, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XVRCTY8 = factor(XVRCTY8, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XVRCTY7 = factor(XVRCTY7, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XVRCTY6 = factor(XVRCTY6, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XVRCTY5 = factor(XVRCTY5, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XVRCTY4 = factor(XVRCTY4, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XVRCTY3 = factor(XVRCTY3, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XVRCTY2 = factor(XVRCTY2, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XVRCTY1 = factor(XVRCTY1, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XROTTY9 = factor(XROTTY9, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XROTTY8 = factor(XROTTY8, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XROTTY7 = factor(XROTTY7, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XROTTY6 = factor(XROTTY6, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XROTTY5 = factor(XROTTY5, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XROTTY4 = factor(XROTTY4, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XROTTY3 = factor(XROTTY3, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XROTTY2 = factor(XROTTY2, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XROTTY1 = factor(XROTTY1, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPOLTY9 = factor(XPOLTY9, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XPOLTY8 = factor(XPOLTY8, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPOLTY7 = factor(XPOLTY7, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPOLTY6 = factor(XPOLTY6, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPOLTY5 = factor(XPOLTY5, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPOLTY4 = factor(XPOLTY4, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XPOLTY3 = factor(XPOLTY3, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPOLTY2 = factor(XPOLTY2, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPOLTY1 = factor(XPOLTY1, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPCVTY9 = factor(XPCVTY9, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPCVTY8 = factor(XPCVTY8, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XPCVTY7 = factor(XPCVTY7, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPCVTY6 = factor(XPCVTY6, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPCVTY5 = factor(XPCVTY5, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPCVTY4 = factor(XPCVTY4, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPCVTY3 = factor(XPCVTY3, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XPCVTY2 = factor(XPCVTY2, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XPCVTY1 = factor(XPCVTY1, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XMMRTY9 = factor(XMMRTY9, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XMMRTY8 = factor(XMMRTY8, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XMMRTY7 = factor(XMMRTY7, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XMMRTY6 = factor(XMMRTY6, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XMMRTY5 = factor(XMMRTY5, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XMMRTY4 = factor(XMMRTY4, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XMMRTY3 = factor(XMMRTY3, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XMMRTY2 = factor(XMMRTY2, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XMMRTY1 = factor(XMMRTY1, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHIBTY9 = factor(XHIBTY9, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHIBTY8 = factor(XHIBTY8, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHIBTY7 = factor(XHIBTY7, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHIBTY6 = factor(XHIBTY6, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XHIBTY5 = factor(XHIBTY5, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHIBTY4 = factor(XHIBTY4, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHIBTY3 = factor(XHIBTY3, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHIBTY2 = factor(XHIBTY2, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHIBTY1 = factor(XHIBTY1, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XHEPTY9 = factor(XHEPTY9, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHEPTY8 = factor(XHEPTY8, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHEPTY7 = factor(XHEPTY7, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHEPTY6 = factor(XHEPTY6, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHEPTY5 = factor(XHEPTY5, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XHEPTY4 = factor(XHEPTY4, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHEPTY3 = factor(XHEPTY3, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHEPTY2 = factor(XHEPTY2, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XHEPTY1 = factor(XHEPTY1, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XFLUTY9 = factor(XFLUTY9, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XFLUTY8 = factor(XFLUTY8, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XFLUTY7 = factor(XFLUTY7, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XFLUTY6 = factor(XFLUTY6, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XFLUTY5 = factor(XFLUTY5, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XFLUTY4 = factor(XFLUTY4, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XFLUTY3 = factor(XFLUTY3, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XFLUTY2 = factor(XFLUTY2, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XFLUTY1 = factor(XFLUTY1, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XDTPTY9 = factor(XDTPTY9, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XDTPTY8 = factor(XDTPTY8, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XDTPTY7 = factor(XDTPTY7, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XDTPTY6 = factor(XDTPTY6, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XDTPTY5 = factor(XDTPTY5, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XDTPTY4 = factor(XDTPTY4, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(XDTPTY3 = factor(XDTPTY3, levels = TYPElevels, labels = TYPElabels))
    
    df <- df %>% mutate(XDTPTY2 = factor(XDTPTY2, levels = TYPElevels, labels = TYPElabels))
    df <- df %>% mutate(HEP_BRTH = factor(HEP_BRTH, levels = HEPBRTlevels, labels = HEPBRTlabels))
    df <- df %>% mutate(HEP_FLAG = factor(HEP_FLAG, levels = HEPFLGlevels, labels = HEPFLGlabels))
    df <- df %>% mutate(P_U12VRC = factor(P_U12VRC, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTD431H_ROUT_S = factor(P_UTD431H_ROUT_S, levels = UTDlevels, labels = UTDlabels))
    
    df <- df %>% mutate(PUTD4313 = factor(PUTD4313, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTD431H3_ROUT_S = factor(P_UTD431H3_ROUT_S, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(PUT43133 = factor(PUT43133, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(PU431_314 = factor(PU431_314, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTD431H314_ROUT_S = factor(P_UTD431H314_ROUT_S, levels = UTDlevels, labels = UTDlabels))
    
    df <- df %>% mutate(PU4313314 = factor(PU4313314, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTD431H313_ROUT_S = factor(P_UTD431H313_ROUT_S, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(PU4313313 = factor(PU4313313, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(PU431_31 = factor(PU431_31, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTD431H31_ROUT_S = factor(P_UTD431H31_ROUT_S, levels = UTDlevels, labels = UTDlabels))
    
    df <- df %>% mutate(PU431331 = factor(PU431331, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDTP4 = factor(P_UTDTP4, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDTP3 = factor(P_UTDTP3, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDROT_S = factor(P_UTDROT_S, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDPOL = factor(P_UTDPOL, levels = UTDlevels, labels = UTDlabels))
    
    df <- df %>% mutate(P_UTDPCV = factor(P_UTDPCV, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDPC3 = factor(P_UTDPC3, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDMMX = factor(P_UTDMMX, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDMCV = factor(P_UTDMCV, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDHIB_SHORT_S = factor(P_UTDHIB_SHORT_S, levels = UTDlevels, labels = UTDlabels))
    
    df <- df %>% mutate(P_UTDHIB_ROUT_S = factor(P_UTDHIB_ROUT_S, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDHIB = factor(P_UTDHIB, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDHEPA2 = factor(P_UTDHEPA2, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTDHEP = factor(P_UTDHEP, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(P_UTD431 = factor(P_UTD431, levels = UTDlevels, labels = UTDlabels))
    
    df <- df %>% mutate(P_UTD331 = factor(P_UTD331, levels = UTDlevels, labels = UTDlabels))
    df <- df %>% mutate(CEN_REG = factor(CEN_REG, levels = CENREGlevels, labels = CENREGlabels))
    df <- df %>% mutate(STATE = factor(STATE, levels = STATElevels, labels = STATElabels))
    df <- df %>% mutate(RACE_K = factor(RACE_K, levels = RACE_PUFlevels, labels = RACE_PUFlabels))
    df <- df %>% mutate(AGECPOXR = factor(AGECPOXR, levels = AGECPOXRlevels, labels = AGECPOXRlabels))
    
    df <- df %>% mutate(C1R = factor(C1R, levels = C1Rlevels, labels = C1Rlabels))
    df <- df %>% mutate(C5R = factor(C5R, levels = C5Rlevels, labels = C5Rlabels))
    df <- df %>% mutate(INCQ298A = factor(INCQ298A, levels = INCQ298Alevels, labels = INCQ298Alabels))
    df <- df %>% mutate(RACEETHK = factor(RACEETHK, levels = RACEETHKlevels, labels = RACEETHKlabels))
    df <- df %>% mutate(D6R = factor(D6R, levels = D6Rlevels, labels = D6Rlabels))
    
    df <- df %>% mutate(N_PRVR = factor(N_PRVR, levels = D6Rlevels, labels = D6Rlabels))
    df <- df %>% mutate(FRSTBRN = factor(FRSTBRN, levels = FRSTBRNlevels, labels = FRSTBRNlabels))
    df <- df %>% mutate(BF_FORMR08 = factor(BF_FORMR08, levels = BFFORM08Flevels, labels = BFFORM08Flabels))
    
    df <- df %>% mutate(RENT_OWN = factor(RENT_OWN, levels = RENTOWNlevels, labels = RENTOWNlabels))
    df <- df %>% mutate(NUM_PHONE = factor(NUM_PHONE, levels = NUM_PHONlevels, labels = NUM_PHONlabels))
    df <- df %>% mutate(NUM_CELLS_PARENTS = factor(NUM_CELLS_PARENTS, levels = NUM_PHONlevels, labels = NUM_PHONlabels))
    df <- df %>% mutate(NUM_CELLS_HH = factor(NUM_CELLS_HH, levels = NUM_PHONlevels, labels = NUM_PHONlabels))
    df <- df %>% mutate(MARITAL2 = factor(MARITAL2, levels = MAR_PUF2_levels, labels = MAR_PUF2_labels))
    
    df <- df %>% mutate(P_UTDPCVB13 = factor(P_UTDPCVB13, levels = UTDPCVBlevels, labels = UTDPCVBlabels))
    df <- df %>% mutate(EST_GRANT = factor(EST_GRANT, levels = ESTGRANTlevels, labels = ESTGRANTlabels))
    df <- df %>% mutate(INS_STAT_I = factor(INS_STAT_I, levels = INS_STAT_Ilevels, labels = INS_STAT_Ilabels))
    df <- df %>% mutate(INS_BREAK_I = factor(INS_BREAK_I, levels = INS_BREAK_Ilevels, labels = INS_BREAK_Ilabels))
    df <- df %>% mutate(ESTIAP16 = factor(ESTIAP16, levels = ESTIAP16Flevels, labels = ESTIAP16Flabels))
    
    df <- df %>% mutate(M_AGEGRP2 = factor(M_AGEGRP2, levels = MAGEGRP2_levels, labels = MAGEGRP2_labels))
    return(df)
    #unlabeled factors
    df <- df %>% mutate(dflu5 = factor(dflu5))
    df <- df %>% mutate(dflu6 = factor(dflu6))
    df <- df %>% mutate(dhepb6 = factor(dhepb6))
    df <- df %>% mutate(dhepb7 = factor(dhepb7))
    df <- df %>% mutate(dpcv6 = factor(dpcv6))
    df <- df %>% mutate(dpolio6 = factor(dpolio6))
    df <- df %>% mutate(drot4 = factor(drot4))
    df <- df %>% mutate(dvrc3 = factor(dvrc3))
    df <- df %>% mutate(flu5_age = factor(flu5_age))
    df <- df %>% mutate(flu6_age = factor(flu6_age))
    df <- df %>% mutate(hea4_age = factor(hea4_age))
    df <- df %>% mutate(hep6_age = factor(hep6_age))
    df <- df %>% mutate(pcv6_age = factor(pcv6_age))
    df <- df %>% mutate(pol6_age = factor(pol6_age))
    df <- df %>% mutate(rot4_age = factor(rot4_age))
    df <- df %>% mutate(vrc3_age = factor(vrc3_age))
    df <- df %>% mutate(dhepa4 = factor(dhepa4))
}

