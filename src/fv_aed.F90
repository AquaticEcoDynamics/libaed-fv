!###############################################################################
!#                                                                             #
!# fv_aed.F90 - api version                                                    #
!#                                                                             #
!# Interface for FV (Finite Volume) Hydrodynamic Model to AED modules (libaed) #
!#   Designed for TUFLOW-FV, released by BMT Pty Ltd:                          #
!#   http://www.tuflow.com/Tuflow%20FV.aspx                                    #
!#                                                                             #
!# This is the main interface module that manages the connection with the      #
!# host hydrodynamic model; done through the PUBLIC functions listed below.    #
!#                                                                             #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Developed by :                                                              #
!#     AquaticEcoDynamics (AED) Group                                          #
!#                                                                             #
!# Copyright 2024-2026 : The University of Western Australia                   #
!#                                                                             #
!# Copyright by the AED-team @ UWA under the GNU Public License - www.gnu.org  #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Originally created Sept 2024                                                #
!# Follow updates @ https://github.com/AquaticEcoDynamics/libaed-fv            #
!#                                                                             #
!###############################################################################

#include "aed.h"

#define FV_AED_VERS "2.4.0"

#ifndef DEBUG
#define DEBUG      0
#endif

!###############################################################################
MODULE fv_aed
!-------------------------------------------------------------------------------
 ! USE aed_util
   USE aed_common
   USE aed_api
   USE aed_zones
   USE ieee_arithmetic
!  USE OMP_LIB

   IMPLICIT NONE

   PRIVATE

   PUBLIC init_aed_models,     &
          init_var_aed_models, &
          set_env_aed_models,  &
          do_aed_models,       &
          clean_aed_models

   !#--------------------------------------------------------------------------#
   !# Module Data

   AED_REAL,TARGET :: Kw, Ksed
   AED_REAL,TARGET :: rain_factor = 1.
   AED_REAL,TARGET :: sw_factor   = 1.
   AED_REAL,TARGET :: friction    = 0.

   !# Main arrays storing/pointing to the state and diagnostic variables
   AED_REAL,DIMENSION(:,:),POINTER :: cc,    cc_diag
   AED_REAL,DIMENSION(:),  POINTER :: cc_hz, cc_diag_hz

   !# Arrays for environmental variables not supplied externally.
   AED_REAL,DIMENSION(:,:,:),ALLOCATABLE,TARGET :: z_cc
   AED_REAL,DIMENSION(:,:),  ALLOCATABLE,TARGET :: z_cc_hz
   AED_REAL,DIMENSION(:,:,:),ALLOCATABLE,TARGET :: z_cc_diag
   AED_REAL,DIMENSION(:,:),  ALLOCATABLE,TARGET :: z_cc_diag_hz

   !# Maps of surface, bottom and wet/dry (active) cells
   INTEGER,DIMENSION(:),POINTER :: surf_map, benth_map
   INTEGER,DIMENSION(:),POINTER :: surf_map2, benth_map2
   LOGICAL,DIMENSION(:),POINTER :: active

   !# Maps to nearest cell with water (for riparian exchange)
   AED_REAL,DIMENSION(:),ALLOCATABLE,TARGET :: nearest_active
   AED_REAL,DIMENSION(:),ALLOCATABLE,TARGET :: nearest_depth
   INTEGER, DIMENSION(:),ALLOCATABLE        :: route_table

   !# Arrays for work, vertical movement (ws), and cross-boundary fluxes
   AED_REAL,DIMENSION(:,:),ALLOCATABLE :: flux
   AED_REAL,DIMENSION(:,:),ALLOCATABLE :: ws
   AED_REAL,DIMENSION(:)  ,ALLOCATABLE :: total
   AED_REAL,DIMENSION(:)  ,ALLOCATABLE :: Fsed_setl
   AED_REAL,DIMENSION(:)  ,ALLOCATABLE :: min_
   AED_REAL,DIMENSION(:)  ,ALLOCATABLE :: max_

   !# Arrays for environmental variables (used if they are not supplied externally)
   AED_REAL,DIMENSION(:),ALLOCATABLE,TARGET :: nir
   AED_REAL,DIMENSION(:),ALLOCATABLE,TARGET :: par
   AED_REAL,DIMENSION(:),ALLOCATABLE,TARGET :: uva
   AED_REAL,DIMENSION(:),ALLOCATABLE,TARGET :: uvb

   AED_REAL,DIMENSION(:),POINTER :: lpar
   AED_REAL,TARGET :: col_taub  ! a temp var for bottom stress (computed from ustar_bed)

   !# To support light
   AED_REAL,TARGET :: yearday
   AED_REAL :: part_day_per_step

   !# Name of files being used to load initial values for benthic
   !  or benthic_diag vars, and the horizontal routing table for riparian flows
   CHARACTER(len=128) :: init_values_file = ''
   CHARACTER(len=128) :: route_table_file = ''

   !# External variables
   AED_REAL,TARGET :: dt
   AED_REAL,DIMENSION(:,:),POINTER :: rad
   AED_REAL,DIMENSION(:),  POINTER :: temp
   AED_REAL,DIMENSION(:),  POINTER :: salt
   AED_REAL,DIMENSION(:),  POINTER :: rho
   AED_REAL,DIMENSION(:),  POINTER :: nuh
   AED_REAL,DIMENSION(:),  POINTER :: h
   AED_REAL,DIMENSION(:),  POINTER :: depth
!  AED_REAL,DIMENSION(:),  POINTER :: dz
   AED_REAL,DIMENSION(:),  POINTER :: extc
   AED_REAL,DIMENSION(:),  POINTER :: tss
   AED_REAL,DIMENSION(:),  POINTER :: ss1
   AED_REAL,DIMENSION(:),  POINTER :: ss2
   AED_REAL,DIMENSION(:),  POINTER :: ss3
   AED_REAL,DIMENSION(:),  POINTER :: ss4
   AED_REAL,DIMENSION(:),  POINTER :: biodrag
   AED_REAL,DIMENSION(:),  POINTER :: I_0
   AED_REAL,DIMENSION(:),  POINTER :: wind
   AED_REAL,DIMENSION(:),  POINTER :: air_temp
   AED_REAL,DIMENSION(:),  POINTER :: air_pres
   AED_REAL,DIMENSION(:),  POINTER :: rain
   AED_REAL,DIMENSION(:),  POINTER :: humidity
   AED_REAL,DIMENSION(:),  POINTER :: longwave
   AED_REAL,DIMENSION(:),  POINTER :: area
   AED_REAL,DIMENSION(:),  POINTER :: bathy
   AED_REAL,DIMENSION(:),  POINTER :: rainloss
!  AED_REAL,DIMENSION(:),  POINTER :: solarshade
   AED_REAL,DIMENSION(:),  POINTER :: ustar_bed
   AED_REAL,DIMENSION(:),  POINTER :: wv_uorb
   AED_REAL,DIMENSION(:),  POINTER :: wv_t
!  AED_REAL,DIMENSION(:),  POINTER :: vvel   !# vertical velocity
!  AED_REAL,DIMENSION(:),  POINTER :: cvel   !# cell velocity

   AED_REAL,DIMENSION(:),  POINTER :: layer_stress => null()
   AED_REAL,DIMENSION(:),  POINTER :: sed_zones => null()
   AED_REAL,DIMENSION(:),  POINTER :: sed_zone => null()
!  AED_REAL,DIMENSION(:),  POINTER :: pres => null()

   AED_REAL,DIMENSION(:),ALLOCATABLE,TARGET :: feedback

!##--------------------------------------------------##
!  %% NAMELIST   %%  /aed_bio/
   INTEGER  :: solution_method = 1

   CHARACTER(len=128) :: aed_nml_file = 'aed.nml'

   LOGICAL  :: link_ext_par = .FALSE.
   LOGICAL  :: link_wave_stress = .FALSE.
   LOGICAL  :: link_solar_shade = .TRUE.
   LOGICAL  :: link_rain_loss = .FALSE.
   LOGICAL  :: link_water_clarity = .FALSE.
   LOGICAL  :: link_bottom_drag = .FALSE.
   LOGICAL  :: link_surface_drag = .FALSE.
   LOGICAL  :: link_water_density = .FALSE.

   AED_REAL :: wave_factor =  1.0
   LOGICAL  :: depress_clutch = .FALSE.
   LOGICAL  :: do_limiter = .FALSE.
   LOGICAL  :: do_particle_bgc = .FALSE.
   LOGICAL  :: do_zone_averaging = .FALSE.
   INTEGER  :: benthic_mode = 1
   LOGICAL  :: do_2d_atm_flux = .TRUE.

   !# Switches for configuring model operation and active links with the host model
   AED_REAL :: base_par_extinction = 0.1
   LOGICAL  :: ext_tss_extinction = .FALSE.
   AED_REAL :: tss_par_extinction = 0.2

   !# maximum single precision real is 2**128 = 3.4e38
   AED_REAL :: glob_min = -1.0e38
   AED_REAL :: glob_max =  1.0e38
   LOGICAL  :: no_glob_lim = .FALSE.

   AED_REAL :: min_water_depth =  0.0401
   INTEGER  :: n_equil_substep = 1

   LOGICAL  :: display_minmax = .FALSE.
   INTEGER  :: display_cellid(10) = -99

   AED_REAL :: nir_frac =  0.52   ! 0.51
   AED_REAL :: par_frac =  0.43   ! 0.45
   AED_REAL :: uva_frac =  0.048  ! 0.035
   AED_REAL :: uvb_frac =  0.002  ! 0.005

   AED_REAL,TARGET :: longitude = 0.
   AED_REAL,TARGET :: latitude = 0.
!  AED_REAL :: latlat = 0.
!  %% END NAMELIST   %%  /aed_bio/
!##--------------------------------------------------##

   AED_REAL,DIMENSION(:),ALLOCATABLE,TARGET :: colnums, mat

   !# Misc variables/options
   LOGICAL  :: request_nearest = .FALSE.
   LOGICAL  :: have_nearest = .FALSE.
   INTEGER  :: ThisStep = 0
   INTEGER  :: n_cellids = 0

   !# Integers storing number of variables being simulated
   INTEGER :: n_aed_vars, n_vars, n_vars_ben, n_vars_diag, n_vars_diag_sheet

   INTEGER, DIMENSION(:), ALLOCATABLE :: zm
   INTEGER :: n_cols, n_zones, nCells

CONTAINS
!===============================================================================


!###############################################################################
SUBROUTINE init_aed_models(namlst, dname, nwq_var, nben_var, ndiag_var,        &
                                                     names, bennames, diagnames)
!-------------------------------------------------------------------------------
! This routine is called by the AED library host (TUFLOW-FV) to define numbers
! and names of variables. The host must then allocate the variables arrays
! after return from this routine.
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,          INTENT(in)  :: namlst
   INTEGER,          INTENT(out) :: nwq_var,nben_var,ndiag_var
   CHARACTER(len=*), INTENT(in)  :: dname
   CHARACTER(len=30),ALLOCATABLE,INTENT(out) :: names(:)
   CHARACTER(len=30),ALLOCATABLE,INTENT(out) :: bennames(:)
   CHARACTER(len=30),ALLOCATABLE,INTENT(out) :: diagnames(:)
!
!LOCALS
   TYPE(aed_variable_t),POINTER :: tvar
   CHARACTER(len=128)           :: tname, line
   INTEGER                      :: status, n_sd, i, j, tv
   INTEGER                      :: sz_n, sz_bn, sz_dn

   TYPE(aed_coupling_t) :: cpl

   AED_REAL :: latlat = 0.
   INTEGER  :: split_factor = 1
   LOGICAL  :: mobility_off = .FALSE.
   LOGICAL  :: bioshade_feedback = .FALSE.
   LOGICAL  :: repair_state = .TRUE.

   CHARACTER(len=64) :: models(64)

   NAMELIST /aed_models/ models
   NAMELIST /aed_bio/ solution_method, aed_nml_file, link_bottom_drag,         &
                      link_surface_drag, link_water_density,                   &
                      link_water_clarity,                                      &
                      link_ext_par, base_par_extinction,                       &
                      ext_tss_extinction, tss_par_extinction,                  &
                      do_particle_bgc, do_2d_atm_flux, do_zone_averaging,      &
                      link_solar_shade, link_rain_loss, init_values_file,      &
                      do_limiter, glob_min, glob_max, no_glob_lim,             &
                      route_table_file, n_equil_substep, min_water_depth,      &
                      link_wave_stress, wave_factor, display_minmax,           &
                      display_cellid, depress_clutch,                          &
                      nir_frac,par_frac,uva_frac,uvb_frac, longitude, latlat
!
!-------------------------------------------------------------------------------
!BEGIN
   print *, " "
   print *, "    using fv_aed version ", TRIM(FV_AED_VERS)

   ! Process input file (aed.nml) to get run options
   tname = TRIM(dname)//TRIM(aed_nml_file)
   print *,"    reading fv_aed config from ",TRIM(tname)
   OPEN(namlst,file=tname,action='read',status='old',iostat=status)
   IF ( status /= 0 ) STOP "Cannot open file " // TRIM(tname)
   READ(namlst,nml=aed_bio,iostat=status)
   IF ( status /= 0 ) STOP "Cannot read namelist entry aed_bio"

   latitude = latlat
   Kw = base_par_extinction
   Ksed = tss_par_extinction
   print *,'    link options configured between TFV & AED - '
   print *,'        link_ext_par       :  ',link_ext_par
   print *,'        link_water_clarity :  ',link_water_clarity
   print *,'        link_surface_drag  :  ',link_surface_drag,' (not implemented)'
   print *,'        link_bottom_drag   :  ',link_bottom_drag
   print *,'        link_wave_stress   :  ',link_wave_stress
   print *,'        link_solar_shade   :  ',link_solar_shade
   print *,'        link_rain_loss     :  ',link_rain_loss
   print *,'        link_particle_bgc  :  ',do_particle_bgc,' (under development)'
   print *,'        link_water_density :  ',link_water_density,' (not implemented)'

   cpl%glm_style_zones = .FALSE.

   cpl%par_fraction =  0.450
   cpl%nir_fraction =  0.510
   cpl%uva_fraction =  0.035
   cpl%uvb_fraction =  0.005

   cpl%mobility_off = mobility_off
   cpl%bioshade_feedback = bioshade_feedback
   cpl%link_rain_loss = link_rain_loss
   cpl%link_solar_shade = link_solar_shade
   cpl%link_bottom_drag = link_bottom_drag

   cpl%repair_state = repair_state
   cpl%split_factor = split_factor
   cpl%benthic_mode = benthic_mode

   cpl%rain_factor => rain_factor
   cpl%sw_factor => sw_factor
   cpl%friction => friction

   cpl%Kw => Kw

   cpl%do_particle_bgc = do_particle_bgc

   cpl%link_ext_par = link_ext_par

   CALL aed_set_coupling(cpl)

   n_aed_vars = aed_configure_models(tname, n_vars, n_vars_ben, n_vars_diag, n_vars_diag_sheet)
   nwq_var = n_vars
   nben_var = n_vars_ben
   ndiag_var = n_vars_diag + n_vars_diag_sheet
   n_sd = n_vars_diag_sheet

   !# names = grab the names from info
   ALLOCATE(names(1:nwq_var),stat=status)
   IF (status /= 0) STOP 'allocate_memory(): ERROR allocating (names)'
   ALLOCATE(bennames(1:nben_var),stat=status)
   IF (status /= 0) STOP 'allocate_memory(): ERROR allocating (bennames)'
   IF ( .NOT. ALLOCATED(diagnames) ) ALLOCATE(diagnames(ndiag_var))
   IF (status /= 0) STOP 'allocate_memory(): ERROR allocating (diagnames)'

   ALLOCATE(min_(1:nwq_var+nben_var)) ; ALLOCATE(max_(1:nwq_var+nben_var))

   sz_n = 30 !sizeof(names(1))
   sz_bn = 30 !sizeof(bennames(1))
   sz_dn = 30 !sizeof(diagnames(1))

   j = 0
   DO i=1,n_aed_vars
      IF ( aed_get_var(i, tvar) ) THEN
         IF ( .NOT. (tvar%sheet .OR. tvar%diag .OR. tvar%extern) ) THEN
            j = j + 1
            IF ( j > nwq_var ) THEN
                print*, " ERROR - finding more variables than reported"
                EXIT
            ENDIF
            names(j) = TRIM(tvar%name(1:sz_n))
            min_(j) = tvar%minimum
            max_(j) = tvar%maximum
            line = '' ; IF(tvar%zavg) line = '  (zavg)'
            print *,"     S(",j,") AED pelagic(3D) variable: ", TRIM(names(j))//TRIM(line)
         ENDIF
      ENDIF
   ENDDO

   j = 0
   DO i=1,n_aed_vars
      IF ( aed_get_var(i, tvar) ) THEN
         IF ( tvar%sheet .AND. .NOT. (tvar%diag .OR. tvar%extern) ) THEN
            j = j + 1
            IF ( j > nwq_var ) THEN
                print*, " ERROR - finding more benthic variables than reported"
                EXIT
            ENDIF
            bennames(j) = TRIM(tvar%name(1:sz_bn))
            min_(nwq_var+j) = tvar%minimum
            max_(nwq_var+j) = tvar%maximum
            line = '' ; IF(tvar%zavg) line = '  (zavg)'
            print *,"     B(",j,") AED benthic(2D) variable: ", TRIM(bennames(j))//TRIM(line)
         ENDIF
      ENDIF
   ENDDO

   j = 0
   DO i=1,n_aed_vars
      IF ( aed_get_var(i, tvar) ) THEN
         IF ( tvar%diag ) THEN
            j = j + 1
            IF ( j > ndiag_var+n_sd ) THEN
                print*, " ERROR - finding more diagnostic variables than reported"
                EXIT
            ENDIF
            diagnames(j) = TRIM(tvar%name(1:sz_dn))
            line = '' ; IF(tvar%zavg) line = '  (zavg)'
            print *,"     D(",j,") AED diagnostic variable:  ", TRIM(diagnames(j))//TRIM(line)
         ENDIF
      ENDIF
   ENDDO

   CLOSE(namlst)

   DO i=1,10
     IF ( display_cellid(i) /= -99 ) THEN
         n_cellids = n_cellids + 1
     ELSE
         EXIT
     ENDIF
   ENDDO
END SUBROUTINE init_aed_models
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE init_var_aed_models(nCells_, cc_, cc_diag_, nwq, nwqben, sm, bm)
!-------------------------------------------------------------------------------
! Points the AED main variable arrays to those provided by the host model.
! At this point TuflowFV should have allocated the variable space.
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in)                         :: nCells_
   AED_REAL,POINTER,DIMENSION(:,:),INTENT(in) :: cc_, cc_diag_
   INTEGER,INTENT(inout)                      :: nwq, nwqben
   INTEGER,POINTER,DIMENSION(:),INTENT(in)    :: sm, bm
!
!LOCALS
   INTEGER :: rc, av, v, sv, d, sd
   TYPE(aed_variable_t),POINTER :: tv
!
!-------------------------------------------------------------------------------
!BEGIN
   nwq = n_vars
   nwqben = n_vars_ben
   nCells = nCells_

   print *,'    init_var_aed_models : nwq = ',nwq,' nwqben = ',nwqben

   cc => cc_
   cc_hz => cc_(nwq:,1)
   cc_diag => cc_diag_
   cc_diag_hz => cc_diag_(n_vars_diag:,1)
   surf_map => sm
   benth_map => bm

   ! Allocate state and diagnostic variable arrays
   IF ( .NOT. ASSOCIATED(cc) ) STOP ' ERROR : no association for (cc)'
   cc = 0.
   IF (.not. ASSOCIATED(cc_diag) ) STOP ' ERROR : no association for (cc_diag)'
   cc_diag = 0.

   ! Allocate array with vertical movement rates (m/s, positive for upwards)
   ALLOCATE(ws(1:nCells,1:n_aed_vars),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): ERROR allocating (ws)'
   ws = 0.

   !!# place holder for lagranigan particles
   !IF(do_particle_bgc) THEN
   !  pp => pp_
   !END IF

   ! Allocate array for photosynthetically active radiation (PAR).
   ! This will be calculated internally during each time step.
   ALLOCATE(par(1:nCells),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): ERROR allocating (par)'
   par = 0.
   ALLOCATE(nir(1:nCells),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): ERROR allocating (nir)'
   nir = 0.
   ALLOCATE(uva(1:nCells),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): ERROR allocating (uva)'
   uva = 0.
   ALLOCATE(uvb(1:nCells),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): ERROR allocating (uvb)'
   uvb = 0.

   !# Allocate array for sedimentation fluxes and initialize these to zero (no flux).
   ALLOCATE(Fsed_setl(1:nCells),stat=rc)
   IF (rc /= 0) STOP 'allocate_memory(): ERROR allocating (Fsed_setl)'
   Fsed_setl = 0.

   !# Now set initial values
   v = 0 ; sv = 0;
   DO av=1,n_aed_vars
      IF ( .NOT.  aed_get_var(av, tv) ) STOP "ERROR getting variable info"
      IF ( .NOT. ( tv%extern .OR. tv%diag) ) THEN  !# neither global nor diagnostic variable
         IF ( tv%sheet ) THEN
            sv = sv + 1
            cc(n_vars+sv, :) = tv%initial
         ELSE
            v = v + 1
            cc(v,:) = tv%initial
         ENDIF
      ENDIF
   ENDDO

   IF ( init_values_file /= '' ) CALL set_initial_from_file
   IF ( route_table_file /= '' ) CALL load_route_table(ubound(bm, 1))

   ALLOCATE(flux(n_vars+n_vars_ben, nCells),stat=rc) ; IF (rc /= 0) STOP 'allocate_memory(): ERROR allocating (flux)'

!
!-------------------------------------------------------------------------------
CONTAINS

   !############################################################################
   CHARACTER FUNCTION tolower(c)
   !----------------------------------------------------------------------------
   !ARGUMENTS
      CHARACTER, INTENT(in) :: c
   !LOCALS
      INTEGER :: ic
   !BEGIN
   !----------------------------------------------------------------------------
      ic = ichar(c)
      if (ic >= 65 .and. ic < 90) ic = (ic+32)
      tolower = char(ic)
   END FUNCTION tolower
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   !############################################################################
   FUNCTION same_str_icase(a, b) RESULT(res)
   !----------------------------------------------------------------------------
   !ARGUMENTS
      CHARACTER(len=*), INTENT(in) :: a,b
   !LOCALS
      INTEGER :: len, i
      LOGICAL :: res
   !
   !BEGIN
   !----------------------------------------------------------------------------
      res = .FALSE.
      len = LEN_TRIM(a)
      IF ( len /= LEN_TRIM(b) ) RETURN
      DO i=1, len
         if (tolower(a(i:i)) /= tolower(b(i:i)) ) RETURN
      ENDDO
      res = .TRUE.
   END FUNCTION same_str_icase
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   !############################################################################
   SUBROUTINE set_initial_from_file
   !----------------------------------------------------------------------------
   USE aed_csv_reader
   !
   !LOCALS
      INTEGER :: unit, nccols, ccol
      CHARACTER(len=32),POINTER,DIMENSION(:) :: csvnames
      TYPE(AED_SYMBOL),DIMENSION(:),ALLOCATABLE :: values
      INTEGER :: idx_col = 0, numv = 0, numd = 0, t
      INTEGER,DIMENSION(:),ALLOCATABLE :: vars, vmap
      INTEGER,DIMENSION(:),ALLOCATABLE :: dvar, dmap
      LOGICAL,DIMENSION(:),ALLOCATABLE :: vsheet, dsheet
      LOGICAL :: meh
   !
   !BEGIN
   !----------------------------------------------------------------------------
      unit = aed_csv_read_header(init_values_file, csvnames, nccols)
      IF (unit <= 0) RETURN !# No file found
      print *,'    benthic AED var initialisation from file: '
      print *,'        ', TRIM(init_values_file)

      DO ccol=1,nccols
         IF ( csvnames(ccol) == "ID" ) THEN
            idx_col = ccol
            EXIT
         ENDIF
      ENDDO

      ALLOCATE(vars(nccols))   ; ALLOCATE(vmap(nccols))
      ALLOCATE(dvar(nccols))   ; ALLOCATE(dmap(nccols))
      ALLOCATE(vsheet(nccols)) ; ALLOCATE(dsheet(nccols))
      ALLOCATE(values(nccols))
      vmap = 0 ; dmap = 0

      IF ( idx_col > 0 ) THEN
         v = 0 ; sv = 0; d = 0; sd = 0
         DO av=1,n_aed_vars
            IF ( .NOT. aed_get_var(av, tv) ) STOP "ERROR getting variable info"
            IF ( .NOT. ( tv%extern ) ) THEN  !#  dont do environment vars
               IF (tv%diag) THEN
                  d = d + 1
               ELSE
                  IF ( tv%sheet ) THEN
                     sv = sv + 1
                  ELSE
                     v = v + 1
                  ENDIF
               ENDIF
               DO ccol=1,nccols
                  IF ( same_str_icase(tv%name, csvnames(ccol)) ) THEN
                     IF (tv%diag) THEN
                        numd = numd + 1
                        dmap(numd) = ccol
                      ! IF ( same_str_icase(tv%name, "LND_phreatic") ) THEN
                      ! phreat_id = av ; phreat_col = ccol ; phreat_var = d ; ENDIF
                        dvar(numd) = d
                        dsheet(numd) = tv%sheet
                     ELSE
                        numv = numv + 1
                        vmap(numv) = ccol
                        IF ( tv%sheet ) THEN
                           vars(numv) = n_vars + sv
                        ELSE
                           vars(numv) = v
                        ENDIF
                        vsheet(numv) = tv%sheet
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO

         DO WHILE ( aed_csv_read_row(unit, values) )
            t = extract_integer(values(idx_col))
            DO v=1,numv
               IF ( vmap(v) == 0 ) CYCLE
               If ( vsheet(v) ) THEN
                  cc(vars(v), bm(t)) = extract_double(values(vmap(v)))
               ELSE
                  cc(vars(v), sm(t):bm(t)) = extract_double(values(vmap(v)))
               ENDIF
            ENDDO
            DO v=1,numd
               IF ( dmap(v) == 0 ) CYCLE
               ! IF (dmap(v) == phreat_col ) &
               ! print*, " XXX setting phreat_col ", phreat_var
               If ( vsheet(v) ) THEN
                  cc_diag(dvar(v), bm(t)) = extract_double(values(dmap(v)))
               ELSE
                  cc_diag(dvar(v), sm(t):bm(t)) = extract_double(values(dmap(v)))
               ENDIF
            ENDDO
         ENDDO
      ENDIF

      meh = aed_csv_close(unit) !# don't care if close fails

      IF (ASSOCIATED(csvnames)) DEALLOCATE(csvnames)
      IF (ALLOCATED(values))    DEALLOCATE(values)
      IF (ALLOCATED(vars))      DEALLOCATE(vars)
      IF (ALLOCATED(vmap))      DEALLOCATE(vmap)
      IF (ALLOCATED(dvar))      DEALLOCATE(dvar)
      IF (ALLOCATED(dmap))      DEALLOCATE(dmap)
   END SUBROUTINE set_initial_from_file
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   !############################################################################
   SUBROUTINE load_route_table(nrows)
   !----------------------------------------------------------------------------
   USE aed_csv_reader
   !ARGUMENTS
      INTEGER,INTENT(in) :: nrows
   !
   !LOCALS
      INTEGER :: unit, nccols, ccol, crow
      CHARACTER(len=32),POINTER,DIMENSION(:) :: csvnames
      TYPE(AED_SYMBOL),DIMENSION(:),ALLOCATABLE :: values
      INTEGER :: idx_col = 0, t
      LOGICAL :: meh
   !
   !BEGIN
   !----------------------------------------------------------------------------
      unit = aed_csv_read_header(route_table_file, csvnames, nccols)
      IF (unit <= 0) RETURN !# No file found
      print *,'    riparian cell routing set from file: '
      print *,'        ', TRIM(route_table_file)

   !# The format of the file should be me, "lowest ajoining" - ie always 2 colums
   !# and always in the order - and we dont really care about the header, but
   !# being csv it should have it so we read but ignore it.
   !     DO ccol=1,nccols
   !        IF ( csvnames(ccol) == "ID" ) THEN
   !           idx_col = ccol
   !           EXIT
   !        ENDIF
   !     ENDDO
   !     IF (idx_col == 0) THEN
   !        print*,"Could not find column 'ID'"
   !        RETURN
   !     ENDIF
      idx_col = 1

      ALLOCATE(values(nccols))
      ALLOCATE(route_table(nrows))
      ALLOCATE(nearest_active(nrows))
      ALLOCATE(nearest_depth(nrows))
      route_table = 0

      crow = 0
      DO WHILE ( aed_csv_read_row(unit, values) )
         crow = crow + 1
         IF ( crow > nrows ) THEN
            print*, "        NOTE: routing table has more rows than expected - extras ignored"
         ENDIF
         t = extract_integer(values(idx_col))
         route_table(crow) = extract_integer(values(2))

         !MH PUT A CHECK HERE TO MAKE SURE NO CIRCULAR REFERENCE
      ENDDO

      IF ( crow < nrows ) &
      print*, "        NOTE: routing table has less rows than expected? ",crow,"/",nrows

      meh = aed_csv_close(unit)  !# don't care if close fails

      IF (ASSOCIATED(csvnames)) DEALLOCATE(csvnames)
      IF (ALLOCATED(values))    DEALLOCATE(values)
      have_nearest = .TRUE.
   END SUBROUTINE load_route_table
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

END SUBROUTINE init_var_aed_models
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE set_env_aed_models(dt_,              &
                            ! 3D env variables
                               temp_,            &
                               salt_,            &
                               rho_,             &
                               h_,               &
                               tss_,             &
                               rad_,             &
                               vvel_,            &
                               cvel_,            &
                            ! 3D feedback arrays
                               extcoeff_,        &
                            ! 2D env variables
                               area_,            &
                               I_0_,             &
                               longwave_,        &
                               wnd_,             &
                               rain_,            &
                               humidity_,        &
                               air_temp_,        &
                               ustar_bed_,       &
                               ustar_surf_,      &
                               wv_uorb_,         &
                               wv_t_,            &
                               z_,               &
                               bathy_,           &
                               mat_id_,          &
                               active_,          &
                            ! 2D feedback arrays
                               biodrag_,         &
                               solarshade_,      &
                               rainloss_,        &
                            ! some extra for light
                               time, lat_,       &
                            ! and more
                               air_pres_         &
                              )
!-------------------------------------------------------------------------------
! Provide environmental information from TuflowFV and set feedback arrays
!-------------------------------------------------------------------------------
!ARGUMENTS
   DOUBLETYPE, INTENT(in) :: dt_
   AED_REAL, INTENT(in), DIMENSION(:),   POINTER :: temp_, salt_, rho_, h_,    &
                                                    area_, tss_, extcoeff_, z_
   AED_REAL, INTENT(in), DIMENSION(:,:), POINTER :: rad_
   AED_REAL, INTENT(in), DIMENSION(:),   POINTER :: vvel_, cvel_
   AED_REAL, INTENT(in), DIMENSION(:),   POINTER :: I_0_, wnd_, ustar_bed_, ustar_surf_
   AED_REAL, INTENT(in), DIMENSION(:),   POINTER :: longwave_
   AED_REAL, INTENT(in), DIMENSION(:),   POINTER :: wv_uorb_, wv_t_
   AED_REAL, INTENT(in), DIMENSION(:),   POINTER :: rain_, bathy_
   AED_REAL, INTENT(in), DIMENSION(:),   POINTER :: air_temp_
   AED_REAL, INTENT(in), DIMENSION(:),   POINTER :: humidity_
   INTEGER,  INTENT(in), DIMENSION(:,:), POINTER :: mat_id_
   LOGICAL,  INTENT(in), DIMENSION(:),   POINTER :: active_
   AED_REAL, INTENT(in), DIMENSION(:),   POINTER :: biodrag_, solarshade_, rainloss_
   AED_REAL, INTENT(in)                          :: time
   AED_REAL, INTENT(in)                          :: lat_
   AED_REAL, INTENT(in), DIMENSION(:),   POINTER :: air_pres_
!
!LOCALS
   INTEGER :: col, top, bot, lev, base
   INTEGER :: nTypes, cType, zon, n_layers
   INTEGER, DIMENSION(:),ALLOCATABLE :: mat_t
   TYPE(aed_env_t),DIMENSION(:),ALLOCATABLE :: aed_env
   TYPE(aed_data_t),DIMENSION(:),ALLOCATABLE :: aed_data
   AED_REAL :: surf

   AED_REAL,DIMENSION(:),ALLOCATABLE :: tst

   PROCEDURE(aed_mobility_fn_t),POINTER :: doMobilityP
!
!-------------------------------------------------------------------------------
!BEGIN
   print *,'    set_env_aed_models : linking to host environment vars '

   !# Provide pointers to arrays with environmental variables to AED.
   dt = dt_
   part_day_per_step = dt / 86400.
!  yearday = day_of_year(time) ! calc from time

   n_cols = ubound(mat_id_,2)
   n_layers = 0
   DO col=1, n_cols
      lev = ABS(surf_map(col) - benth_map(col)) + 1
      if ( lev > n_layers ) n_layers = lev
   ENDDO
!print*,"n_cols = ",n_cols," n_layers = ",n_layers

   !# 2D (sheet) variables being pointed to
!  area => area_
   ALLOCATE(area(n_cols*n_layers))
!  ustar_bed => ustar_bed_
   ALLOCATE(ustar_bed(n_cols*n_layers))
   ALLOCATE(wv_uorb(n_cols*n_layers))
   IF (link_wave_stress) THEN
     ALLOCATE(wv_t(n_cols*n_layers))
!    wv_uorb => wv_uorb_
!    wv_t => wv_t_
   ENDIF
   ALLOCATE(sed_zone(n_cols))
   ALLOCATE(sed_zones(n_cols*n_layers))
!  biodrag => biodrag_
   ALLOCATE(biodrag(n_cols*n_layers))
!  solarshade => solarshade_
!  ALLOCATE(solarshade(n_cols*n_layers))
   ALLOCATE(surf_map2(n_cols)) ; ALLOCATE(benth_map2(n_cols))
   DO col=1, n_cols
      base = benth_map(col)
      IF (surf_map(col) < benth_map(col)) base = surf_map(col)
      surf_map2(col) = surf_map(col) - base + 1
      benth_map2(col) = benth_map(col) - base + 1

      IF ( surf_map(col) > benth_map(col) ) THEN
         area(benth_map(col):surf_map(col)) = area_(col)
         ustar_bed(benth_map(col):surf_map(col)) = ustar_bed_(col)
         IF (link_wave_stress) THEN
            wv_uorb(benth_map(col):surf_map(col)) = wv_uorb_(col)
            wv_t(benth_map(col):surf_map(col)) = wv_t_(col)
         ENDIF
         biodrag(benth_map(col):surf_map(col)) = biodrag_(col)
!        solarshade(benth_map(col):surf_map(col)) = solarshade_(col)
      ELSE
         area(surf_map(col):benth_map(col)) = area_(col)
         ustar_bed(surf_map(col):benth_map(col)) = ustar_bed_(col)
         IF (link_wave_stress) THEN
            wv_uorb(surf_map(col):benth_map(col)) = wv_uorb_(col)
            wv_t(surf_map(col):benth_map(col)) = wv_t_(col)
         ENDIF
         biodrag(surf_map(col):benth_map(col)) = biodrag_(col)
!        solarshade(benth_map(col):surf_map(col)) = solarshade_(col)
      ENDIF
   ENDDO
   I_0 => I_0_
   longwave => longwave_
   wind => wnd_
   bathy => bathy_
   rain  => rain_
   rainloss => rainloss_
   air_temp => air_temp_
   IF ( .NOT.ASSOCIATED(air_pres_) ) THEN
     air_pres => air_pres_
   ELSE
     ALLOCATE(air_pres(n_cols))
     air_pres = 1013.25
   ENDIF
   humidity => humidity_

   ALLOCATE(aed_env(n_cols))
   ALLOCATE(aed_data(n_cols))
   ALLOCATE(colnums(n_cols))
   ALLOCATE(mat(n_cols))
   ALLOCATE(mat_t(n_cols))
   ALLOCATE(zm(n_cols))
   ALLOCATE(layer_stress(n_cols))
   ALLOCATE(feedback(n_cols))

   !# 3D variables being pointed to
   h => h_           !# layer heights [1d array] needed for advection, diffusion
   depth => z_       !# depth [1d array], used to calculate local pressure
   extc => extcoeff_ !# biogeochemical light attenuation coefficients [1d array],
                     !# output of biogeochemistry, input for physics
   salt => salt_
   temp => temp_

!  vvel => vvel_
!  cvel => cvel_

   rho => rho_
   tss => tss_
   active => active_

   IF (link_ext_par) lpar => rad_(1,:)

   cType = mat_id_(1,1) ; nTypes = 1 ; mat_t(nTypes) = mat_id_(1,1)

   DO col=1, n_cols
      top = surf_map(col)
      bot = benth_map(col)

      colnums(col) = col
      mat(col) = REAL(mat_id_(1, col))

      IF ( cType /= mat_id_(1, col) ) THEN
         DO zon=1,nTypes
            IF ( mat_t(zon) .EQ. mat_id_(1, col) ) THEN
               cType = mat_id_(1, col)
               EXIT
            ENDIF
         ENDDO     
      ENDIF
      IF ( cType /= mat_id_(1, col) ) THEN
         nTypes = nTypes + 1
         mat_t(nTypes) = mat_id_(1, col)
         cType = mat_id_(1, col)
         zon = nTypes
         zm(col) = zon
      ENDIF

      aed_env(col)%yearday      => yearday
      aed_env(col)%timestep     => dt !timestep

      aed_env(col)%longitude    => longitude
      aed_env(col)%latitude     => latitude

      aed_env(col)%top_idx      => surf_map2(col)
      aed_env(col)%bot_idx      => benth_map2(col)
      aed_env(col)%active       => active(col)

      aed_env(col)%temp         => temp(top:bot)
      aed_env(col)%salt         => salt(top:bot)
      aed_env(col)%rho          => rho(top:bot)
      aed_env(col)%dz           => z_(top:bot)
      aed_env(col)%height       => h(top:bot)
      aed_env(col)%area         => area(top:bot)
      aed_env(col)%depth        => depth(top:bot)
      aed_env(col)%extc         => extc(top:bot)
      aed_env(col)%tss          => tss(top:bot)
      aed_env(col)%ss1          => ss1(top:bot)
      aed_env(col)%ss2          => ss2(top:bot)
      aed_env(col)%ss3          => ss3(top:bot)
      aed_env(col)%ss4          => ss4(top:bot)
      aed_env(col)%cvel         => cvel_(top:bot)
      aed_env(col)%rad          => rad_(:,col)

      aed_env(col)%I_0          => I_0(col)
      aed_env(col)%wind         => wind(col)
      aed_env(col)%air_temp     => air_temp(col)
      aed_env(col)%air_pres     => air_pres(col)
      aed_env(col)%rain         => rain(col)
      aed_env(col)%humidity     => humidity(col)
      aed_env(col)%longwave     => longwave(col)
      aed_env(col)%bathy        => bathy(col)
      aed_env(col)%rainloss     => rainloss(col)
      aed_env(col)%layer_stress => layer_stress(col)

      aed_env(col)%ustar_bed    => ustar_bed(top:bot)
      aed_env(col)%wv_uorb      => wv_uorb(top:bot)
      aed_env(col)%wv_t         => wv_t(top:bot)

      aed_env(col)%sed_zones    => sed_zones(top:bot)
      aed_env(col)%sed_zone     => sed_zone(col)

      aed_env(col)%par          => par(top:bot)
      aed_env(col)%nir          => nir(top:bot)
      aed_env(col)%uva          => uva(top:bot)
      aed_env(col)%uvb          => uvb(top:bot)

!     aed_env(col)%pres         => pres(top:bot)

      aed_env(col)%biodrag      => biodrag(top:bot)
      aed_env(col)%solarshade   => solarshade_(col)

      aed_env(col)%windshade    => feedback(col)

      aed_data(col)%cc          => cc(:, top:bot)
      aed_data(col)%cc_hz       => cc_hz(:)
      aed_data(col)%cc_diag     => cc_diag(:, top:bot)
      aed_data(col)%cc_diag_hz  => cc_diag_hz(:)
   ENDDO

   CALL aed_set_model_env(aed_env, n_cols, n_layers)
   DEALLOCATE(aed_env)

   CALL aed_set_model_data(aed_data, n_cols, n_layers)
   DEALLOCATE(aed_data)

   IF (n_zones .GT. 0) &
      CALL api_set_fv_zones(n_layers, n_cols, n_vars, n_vars_ben, n_vars_diag, n_vars_diag_sheet, n_aed_vars)

!  CALL init_zones(ubound(mat_id_, 2), mat_id_, do_zone_averaging, n_vars, n_vars_ben, n_vars_diag)

!print*,"allocating all_parts with ", ubound(temp,1), " cells"
!  ALLOCATE(all_particles(n_cols))

   doMobilityP => doMobilityF
   CALL aed_set_mobility_fn(doMobilityP)
END SUBROUTINE set_env_aed_models
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE fill_nearest(n_cols)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER, INTENT(in) :: n_cols
!
!LOCALS
   INTEGER  :: k, col, prev, next
!
!-------------------------------------------------------------------------------
!BEGIN
   IF ( ALLOCATED(route_table) ) THEN
      DO col=1, n_cols
         IF (active(col) .AND. h(benth_map(col))>=min_water_depth) THEN
            nearest_active(col) = col
            nearest_depth(col) = h(benth_map(col)) + bathy(col)
         ELSE
            k = route_table(col)
            DO WHILE ( .NOT. active(k) .OR. h(benth_map(k))<min_water_depth)
               IF ( k == route_table(k) ) EXIT
               k = route_table(k)
            ENDDO
            nearest_active(col) = k
            nearest_depth(col) = h(benth_map(k)) + bathy(k)
            ! this needs fixing to sum over top:bot, as h is layer thicknesses, not references to datum
         ENDIF
      ENDDO
   ELSE
      nearest_active = 0.
   ENDIF
END SUBROUTINE fill_nearest
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE do_aed_models(nCells_, n_cols, time)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER, INTENT(in) :: nCells_, n_cols
   AED_REAL,INTENT(in) :: time
!
!LOCALS
   TYPE(aed_variable_t),POINTER :: tv

   INTEGER :: i, j, col, lev, v, d
   AED_REAL,PARAMETER :: r100 = 1.0e2
   INTEGER :: grp, prt, stat, idx3d
!aed_real :: surf
!
!-------------------------------------------------------------------------------
!BEGIN
   !# for debugging, depress flag doesn't run the aed library - allows us to
   !#  see how much time is used by libaed calculations by not doing them
   IF (depress_clutch) return

!$OMP BARRIER
!$OMP SINGLE
   print *,"    START do_aed_models"

   !#--------------------------------------------------------------------
   !# START-UP JOBS
   rainloss = zero_

   yearday = day_of_year(time) ! calc from time

   IF ( request_nearest ) CALL fill_nearest(n_cols)

!  IF ( .NOT. reinited )  CALL re_initialize()

   ThisStep = ThisStep + 1

   ! if bio-active particles are running, update particle data
#if 0
   IF (do_particle_bgc) THEN
      DO i=1, ubound(all_particles)
         IF (ALLOCATED(all_particles(i)%prt)) DEALLOCATE(all_particles(i)%prt)
         all_particles(i)%count = 0
      ENDDO
      DO grp=1,num_groups
         stat = particle_groups(grp)%id_stat  ! should be 1
         idx3d = particle_groups(grp)%id_i3   ! should be 3
         DO prt=1,particle_groups(grp)%NP
            IF ( particle_groups(grp)%istat(stat, prt) >= 0 ) THEN
               i = particle_groups(grp)%istat(idx3d, prt)
               IF ( i >= 1 .AND. i <= ubound(all_particles) ) THEN
                  all_particles(i)%count = all_particles(i)%count + 1
!              ELSE
!                 print*,"idx out of range", i, ubound(all_particles)
!                 stop
               ENDIF
            ENDIF
         ENDDO
!     ENDDO
!     DO grp=1,num_groups
         DO prt=1,particle_groups(grp)%NP
            IF ( particle_groups(grp)%istat(stat, prt) < 0 ) CYCLE  !# ignore these

            i = particle_groups(grp)%istat(idx3d, prt)
            IF ( i >= 1 .AND. i <= ubound(all_particles) ) THEN
               IF (.NOT. ALLOCATED(all_particles(i)%prt)) THEN
                  ALLOCATE(all_particles(i)%prt(all_particles(i)%count))
                  all_particles(i)%n = 0
               ENDIF
               j = all_particles(i)%n + 1
               IF (j <= all_particles(i)%count ) THEN
                  all_particles(i)%prt(j)%grp = grp
                  all_particles(i)%prt(j)%idx = prt
                  all_particles(i)%n = j
!              ELSE
!                 print*,"Ooops, error in PTM", j, all_particles(i)%count
               ENDIF
!           ELSE
!              print*,"idx out of range", i, ubound(all_particles)
!              print*,"grp", grp, " prt ",prt
!              print*,"istat 1", particle_groups(grp)%istat(1,prt)
!              print*,"istat 2", particle_groups(grp)%istat(2,prt)
!              print*,"istat 3", particle_groups(grp)%istat(3,prt)
!              print*,"istat 4", particle_groups(grp)%istat(4,prt)
!              stop
            ENDIF
         ENDDO
      ENDDO
   ENDIF
#endif

!$OMP END SINGLE

   CALL aed_run_model(n_cols, -1, do_2d_atm_flux)

END SUBROUTINE do_aed_models
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#define _UNUSED(x) if (.FALSE.) print*,shape(x)
!###############################################################################
SUBROUTINE doMobilityF(N, dt, h, A, ww, min_C, mcc)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in)     :: N       !# number of vertical layers
   AED_REAL,INTENT(in)    :: dt      !# time step (s)
   AED_REAL,INTENT(in)    :: h(:)    !# layer thicknesses (m)
   AED_REAL,INTENT(in)    :: A(:)    !# layer areas (m2)
   AED_REAL,INTENT(in)    :: ww(:)   !# vertical speed (m/s)
   AED_REAL,INTENT(in)    :: min_C   !# minimum allowed cell concentration
   AED_REAL,INTENT(inout) :: mcc(:)  !# cell concentration
!
!LOCALS
   AED_REAL :: Fsed = 0.
!
!-------------------------------------------------------------------------------
!BEGIN
   _UNUSED(A)

   CALL Settling(N,dt,h,ww,Fsed,mcc)
END SUBROUTINE doMobilityF
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   
   

!###############################################################################
SUBROUTINE Settling(N, dt, h, wvel, Fsed, Y)
!-------------------------------------------------------------------------------
!
! Update settling of AED state variables in a given column
!
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in)     :: N       !# number of vertical layers
   AED_REAL,INTENT(in)    :: dt      !# time step (s)
   AED_REAL,INTENT(in)    :: h(:)    !# layer thickness (m)
   AED_REAL,INTENT(in)    :: wvel(:) !# vertical advection speed
   AED_REAL,INTENT(inout) :: Fsed    !# value of sediment input due to settling
   AED_REAL,INTENT(inout) :: Y(:)
!
!CONSTANTS
   INTEGER,PARAMETER :: itmax=100
!
!LOCALS
   INTEGER  :: i,k,it
   AED_REAL :: step_dt
   AED_REAL :: Yc
   AED_REAL :: c,cmax
   AED_REAL :: cu(N+1)
!
!-------------------------------------------------------------------------------
!BEGIN
   Fsed = 0. !# initialize sediment settling fluxes with zero
   cu   = 0. !# initialize interface fluxes with zero
   cmax = 0. !# initialize maximum Courant number

   !# compute maximum Courant number
   !      calculated as number of layers that the particles will travel based
   !      on settling or buoyancy velocity.
   !      This number is then used to split the vertical movement
   !      calculations to limit movement across a single layer
   DO k=2,N
      !# sinking particles
      c=abs(wvel(k-1))*dt/(0.5*(h(k-1)+h(k)))
      IF (c > cmax) cmax=c
      !# rising particles
      c=abs(wvel(k))*dt/(0.5*(h(k-1)+h(k)))
      IF (c > cmax) cmax=c
   ENDDO

   it=min(itmax,int(cmax)+1)
   step_dt = dt / float(it);

   !# splitting loop
   DO i = 1,it
      !# vertical loop
      DO k=N,2,-1
         !# compute the slope ratio
         IF (wvel(k) > 0.) THEN !# Particle is rising
            Yc=Y(k)       !# central value
         ELSE !# negative speed Particle is sinking
            Yc=Y(k-1)     !# central value
         ENDIF

         !# compute the limited flux
         cu(k)=wvel(k) * Yc
      ENDDO

      !# do the upper boundary conditions
      cu(1) = zero_       !# limit flux into the domain from atmosphere

      !# do the lower boundary conditions
      IF (wvel(N) > 0.) THEN !# Particle is rising
         cu(N+1) = 0.  !flux from benthos is zero
      ELSE  !# Particle is settling
         cu(N+1) = wvel(N)*Y(N)
         Fsed = cu(N+1) * step_dt !# flux settled into the sediments per sub time step
      ENDIF
      !# do the vertical advection step including positive migration
      !# and settling of suspended matter.
      DO k=N,1,-1
          Y(k)=Y(k) - step_dt * ((cu(k) - cu(k+1)) / h(k))
      ENDDO
   ENDDO !# end of the iteration loop
   Fsed = Fsed / dt !# Average flux rate for full time step used in AED
END SUBROUTINE Settling
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE clean_aed_models
!-------------------------------------------------------------------------------
!ARGUMENTS
!
!LOCALS
!
!-------------------------------------------------------------------------------
!BEGIN
   ! Deallocate internal arrays
   IF (allocated(ws))             deallocate(ws)
   IF (allocated(total))          deallocate(total)
   IF (allocated(nir))            deallocate(nir)
   IF (allocated(par))            deallocate(par)
   IF (allocated(uva))            deallocate(uva)
   IF (allocated(uvb))            deallocate(uvb)
!  IF (allocated(pactive))        deallocate(pactive)
END SUBROUTINE clean_aed_models
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
AED_REAL FUNCTION day_of_year(time)
!-------------------------------------------------------------------------------
!ARGUMENTS
  AED_REAL,INTENT(in) :: time
!
!LOCAL VARIABLES:
  integer :: j, jday
  integer :: y, m, d
  integer :: ya, c
  AED_REAL :: frac

!-------------------------------------------------------------------------------
  jday = INT4(time/86400.)
  frac = (time/86400) - jday
!print*, 'time is ', time, 'jday is ',jday, ' and frac is ', frac

  ! # calendar_date(jday,&y,&m,&d);

  j = jday - 1721119
  y = (4 * j - 1) / 146097

  j = 4 * j - 1 - 146097 * y
  d = j / 4
  j = (4 * d + 3) / 1461

  d = 4 * d + 3 - 1461 * j
  d = (d + 4) / 4
  m = (5 * d - 3) / 153

  d = 5 * d - 3 - 153 * m
  d = (d + 5) / 5
  y = 100 * y + j

  if (m < 10) then
      m = m + 3
  else
      m = m - 9;
      y = y + 1;
  endif

!print*, 'date : ', y, '/', m, '/', d

! return jday - julian_day(y,1,1);

  m = 1 ; d = 1

  if (m > 2) then
      m = m - 3
  else
      m = m + 9
      y = y - 1
  endif

  c = y / 100
  ya = y - 100 * c

  day_of_year = (146097 * c) / 4 + (1461 * ya) / 4 + (153 * m + 2) / 5 + d + 1721119

  day_of_year = jday - day_of_year + 1
!print*,'day of year ', day_of_year
  day_of_year = day_of_year + frac
!print*,'day of year with frac ', day_of_year

END FUNCTION day_of_year
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!===============================================================================


!###############################################################################
SUBROUTINE api_set_fv_zones(n_layers, n_columns, numVars, numBenV, numDiagV, numDiagHzV, nAEDvars)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: n_layers, n_columns
   INTEGER,INTENT(in) :: numVars, numBenV, numDiagV, numDiagHzV, nAEDvars
!
!LOCALS
   INTEGER :: zon

   PROCEDURE(copy_to_zone_t),POINTER    :: copy_to
   PROCEDURE(copy_from_zone_t),POINTER  :: copy_from
   PROCEDURE(calc_zone_areas_t),POINTER :: calc_areas
!
!-------------------------------------------------------------------------------
!BEGIN
   n_vars = numVars
   n_vars_ben = numBenV
   n_vars_diag = numDiagV
   n_vars_diag_sheet = numDiagHzV
   n_aed_vars = nAEDvars
   n_cols = n_columns

   ALLOCATE(z_cc(numVars, n_layers, n_zones))       ; z_cc = 0.
   ALLOCATE(z_cc_hz(numBenV, n_zones))              ; z_cc_hz = 0.
   ALLOCATE(z_cc_diag(numDiagV, n_layers, n_zones)) ; z_cc_diag = 0.
   ALLOCATE(z_cc_diag_hz(numDiagHzV, n_zones+1))    ; z_cc_diag_hz = 0.
   ALLOCATE(zm(n_columns))

   CALL aed_init_zones(n_zones, 1, z_cc, z_cc_hz, z_cc_diag, z_cc_diag_hz)

   copy_to => api_copy_to_zone
   copy_from => api_copy_from_zone
   calc_areas => api_calc_zone_areas
   CALL api_set_zone_funcs(copy_to, copy_from, calc_areas)

   DO zon=1,n_zones
      aedZones(zon)%z_env%z_area = zero_
   ENDDO
END SUBROUTINE api_set_fv_zones
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE api_calc_zone_areas(theZones, n_zones, areas, heights, wlev)
!-------------------------------------------------------------------------------
!ARGUMENTS
   TYPE(api_zone_t),DIMENSION(:),INTENT(inout) :: theZones
   INTEGER,INTENT(in) :: n_zones
   AED_REAL,DIMENSION(:),POINTER,INTENT(in) :: areas
   AED_REAL,DIMENSION(:),POINTER,INTENT(in) :: heights
   INTEGER,INTENT(in) :: wlev
!
!LOCALS
   INTEGER :: col, zon
   INTEGER :: dbg = 0 !29
   INTEGER :: zone_count(n_zones)
!
!-------------------------------------------------------------------------------
!BEGIN

   DO zon=1,n_zones
      z_cc(1:n_vars,:,zon) = zero_
      z_cc_diag(:,:,zon) = zero_
      z_cc_diag_hz(:,zon) = zero_

      aedZones(zon)%z_env%z_temp = zero_       
      aedZones(zon)%z_env%z_salt = zero_
      aedZones(zon)%z_env%z_rho = zero_
      aedZones(zon)%z_env%z_rad = zero_
      aedZones(zon)%z_env%z_extc = zero_
      aedZones(zon)%z_env%z_layer_stress = zero_
      aedZones(zon)%z_env%z_tss = zero_
      aedZones(zon)%z_env%z_par = zero_
      aedZones(zon)%z_env%z_nir = zero_
      aedZones(zon)%z_env%z_uva = zero_
      aedZones(zon)%z_env%z_uvb = zero_
      aedZones(zon)%z_env%z_sed_zone = zon
      aedZones(zon)%z_env%z_sed_zones = zon
      aedZones(zon)%z_env%z_vel = zero_

      aedZones(zon)%z_env%z_area = zero_
      aedZones(zon)%z_env%z_height = zero_
      aedZones(zon)%z_env%z_extc   = zero_
      aedZones(zon)%z_env%z_wind = zero_
      aedZones(zon)%z_env%z_rain = zero_
      aedZones(zon)%z_env%z_rainloss = zero_
      aedZones(zon)%z_env%z_air_temp = zero_
      aedZones(zon)%z_env%z_air_pres = zero_
      aedZones(zon)%z_env%z_humidity = zero_
      aedZones(zon)%z_env%z_bathy = zero_
      aedZones(zon)%z_env%z_I_0 = zero_
      aedZones(zon)%z_env%z_longwave = zero_
   !  aedZones(zon)%z_env%z_taub = col_taub
      aedZones(zon)%z_env%z_col_depth = one_
   ENDDO 

   zone_count = 0

   ! loop thru all columns in the mesh
   DO col=1, n_cols
      ! zone number of this column
      zon = zm(col)

    ! if (zone(zon) == 11) &
    ! print*,"ZoneIDX ",zon," zone = ",zone(zon)," Col ",col," area col ",area(col),"TEMP ",temp(col)," is ",active(col)
      IF (.NOT. active(col)) CYCLE

      ! cumulate column into relevant zone vars
      aedZones(zon)%z_env%z_area      = aedZones(zon)%z_env%z_area + area(col)

      aedZones(zon)%z_env%z_temp      = aedZones(zon)%z_env%z_temp + temp(col)
      aedZones(zon)%z_env%z_salt      = aedZones(zon)%z_env%z_salt + salt(col)
      aedZones(zon)%z_env%z_rho       = aedZones(zon)%z_env%z_rho + rho(col)
      aedZones(zon)%z_env%z_height    = aedZones(zon)%z_env%z_height + h(col)
      aedZones(zon)%z_env%z_col_depth = aedZones(zon)%z_env%z_col_depth + depth(col)
      aedZones(zon)%z_env%z_extc      = aedZones(zon)%z_env%z_extc + extc(col)
      aedZones(zon)%z_env%z_tss       = aedZones(zon)%z_env%z_tss + tss(col)
      aedZones(zon)%z_env%z_ss1       = aedZones(zon)%z_env%z_ss1 + tss(col) ! For FV API 2.0 (To be connected to sed_conc)
      aedZones(zon)%z_env%z_ss2       = aedZones(zon)%z_env%z_ss2 + tss(col) ! For FV API 2.0 (To be connected to sed_conc)
      aedZones(zon)%z_env%z_ss3       = aedZones(zon)%z_env%z_ss3 + tss(col) ! For FV API 2.0 (To be connected to sed_conc)
      aedZones(zon)%z_env%z_ss4       = aedZones(zon)%z_env%z_ss4 + tss(col) ! For FV API 2.0 (To be connected to sed_conc)
      aedZones(zon)%z_env%z_nir       = aedZones(zon)%z_env%z_nir + nir(col) ! For FV API 2.0 (To be connected to light)
      aedZones(zon)%z_env%z_par       = aedZones(zon)%z_env%z_par + par(col) ! For FV API 2.0 (To be connected to light)
      aedZones(zon)%z_env%z_uva       = aedZones(zon)%z_env%z_uva + uva(col) ! For FV API 2.0 (To be connected to light)
      aedZones(zon)%z_env%z_uvb       = aedZones(zon)%z_env%z_uvb + uvb(col) ! For FV API 2.0 (To be connected to light)
      aedZones(zon)%z_env%z_wind      = aedZones(zon)%z_env%z_wind + wind(col)
      aedZones(zon)%z_env%z_rain      = aedZones(zon)%z_env%z_rain + rain(col)
      aedZones(zon)%z_env%z_rainloss  = aedZones(zon)%z_env%z_rainloss + rainloss(col)
      aedZones(zon)%z_env%z_air_temp  = aedZones(zon)%z_env%z_air_temp + air_temp(col)
      aedZones(zon)%z_env%z_air_pres  = aedZones(zon)%z_env%z_air_pres + air_pres(col)
      aedZones(zon)%z_env%z_humidity  = aedZones(zon)%z_env%z_humidity + humidity(col)
      aedZones(zon)%z_env%z_bathy     = aedZones(zon)%z_env%z_bathy + bathy(col)
      aedZones(zon)%z_env%z_I_0       = aedZones(zon)%z_env%z_I_0 + I_0(col)
      aedZones(zon)%z_env%z_longwave  = aedZones(zon)%z_env%z_longwave + longwave(col)
     !aedZones(zon)%z_env%z_taub      = aedZones(zon)%z_env%z_taub + col_taub

     ! increment column count
      zone_count(zon) = zone_count(zon) + 1
   ENDDO

   ! finalise the average zone environment values (divide sum by count)
   aedZones(zon)%z_env%z_bathy     =     aedZones(zon)%z_env%z_bathy / zone_count(zon)
   aedZones(zon)%z_env%z_col_depth = aedZones(zon)%z_env%z_col_depth / zone_count(zon)
  !aedZones(zon)%z_env%z_height    =    aedZones(zon)%z_env%z_height / zone_count(zon) ! MH this seems to be missing so just cumulating
   aedZones(zon)%z_env%z_I_0       =       aedZones(zon)%z_env%z_I_0 / zone_count(zon)
   aedZones(zon)%z_env%z_wind      =      aedZones(zon)%z_env%z_wind / zone_count(zon)
   aedZones(zon)%z_env%z_rain      =      aedZones(zon)%z_env%z_rain / zone_count(zon)
   aedZones(zon)%z_env%z_rainloss  =  aedZones(zon)%z_env%z_rainloss / zone_count(zon)
   aedZones(zon)%z_env%z_air_temp  =  aedZones(zon)%z_env%z_air_temp / zone_count(zon)
   aedZones(zon)%z_env%z_air_pres  =  aedZones(zon)%z_env%z_air_pres / zone_count(zon)
   aedZones(zon)%z_env%z_humidity  =  aedZones(zon)%z_env%z_humidity / zone_count(zon)
   aedZones(zon)%z_env%z_longwave  =  aedZones(zon)%z_env%z_longwave / zone_count(zon)
   aedZones(zon)%z_env%z_temp      =      aedZones(zon)%z_env%z_temp / zone_count(zon)
   aedZones(zon)%z_env%z_salt      =      aedZones(zon)%z_env%z_salt / zone_count(zon)
   aedZones(zon)%z_env%z_rho       =       aedZones(zon)%z_env%z_rho / zone_count(zon)
   aedZones(zon)%z_env%z_extc      =      aedZones(zon)%z_env%z_extc / zone_count(zon)
  !aedZones(zon)%z_env%z_taub      =      aedZones(zon)%z_env%z_taub / zone_count(zon) ! MH also seems to be missing but NOT cumulating
   aedZones(zon)%z_env%z_tss       =       aedZones(zon)%z_env%z_tss / zone_count(zon)
   aedZones(zon)%z_env%z_nir       =       aedZones(zon)%z_env%z_nir / zone_count(zon)
   aedZones(zon)%z_env%z_par       =       aedZones(zon)%z_env%z_par / zone_count(zon)
   aedZones(zon)%z_env%z_uva       =       aedZones(zon)%z_env%z_uva / zone_count(zon)
   aedZones(zon)%z_env%z_uvb       =       aedZones(zon)%z_env%z_uvb / zone_count(zon)


   ! clean empty zones   !MH THERE WILL BE A DIVEDE BY ZERO BEFORE THIS, ABOVE.
   DO zon=1,n_zones
     !print *,"zoneidx ",zon," zone ",zone(zon)," count ",zone_count(zon)
      IF (zone_count(zon) == 0) THEN
         aedZones(zon)%z_env%z_area     = zero_
         aedZones(zon)%z_env%z_temp     = zero_
         aedZones(zon)%z_env%z_salt     = zero_
         aedZones(zon)%z_env%z_rho      = zero_
         aedZones(zon)%z_env%z_height   = zero_
         aedZones(zon)%z_env%z_extc     = zero_
         aedZones(zon)%z_env%z_tss      = zero_
         aedZones(zon)%z_env%z_ss1      = zero_
         aedZones(zon)%z_env%z_ss2      = zero_
         aedZones(zon)%z_env%z_ss3      = zero_
         aedZones(zon)%z_env%z_ss4      = zero_
         aedZones(zon)%z_env%z_nir      = zero_
         aedZones(zon)%z_env%z_par      = zero_
         aedZones(zon)%z_env%z_uva      = zero_
         aedZones(zon)%z_env%z_uvb      = zero_
         aedZones(zon)%z_env%z_wind     = zero_
         aedZones(zon)%z_env%z_rain     = zero_
         aedZones(zon)%z_env%z_rainloss = zero_
         aedZones(zon)%z_env%z_air_temp = zero_
         aedZones(zon)%z_env%z_air_pres = zero_
         aedZones(zon)%z_env%z_humidity = zero_
         aedZones(zon)%z_env%z_bathy    = zero_
         aedZones(zon)%z_env%z_I_0      = zero_
         aedZones(zon)%z_env%z_longwave = zero_
        !aedZones(zon)%z_env%z_taub     = zero_
      ENDIF

   ENDDO
END SUBROUTINE api_calc_zone_areas
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE api_copy_to_zone(theZones, n_zones, heights, x_cc, x_cc_hz, x_diag, x_diag_hz, wlev)
!-------------------------------------------------------------------------------
!ARGUMENTS
   TYPE(api_zone_t),DIMENSION(:),INTENT(inout) :: theZones
   INTEGER,INTENT(in) :: n_zones
   AED_REAL,DIMENSION(:),  POINTER,INTENT(in) :: heights
   AED_REAL,DIMENSION(:,:),POINTER,INTENT(in) :: x_cc
   AED_REAL,DIMENSION(:),  POINTER,INTENT(in) :: x_cc_hz
   AED_REAL,DIMENSION(:,:),POINTER,INTENT(in) :: x_diag
   AED_REAL,DIMENSION(:),  POINTER,INTENT(in) :: x_diag_hz
   INTEGER,INTENT(in) :: wlev
!
!LOCALS
   INTEGER :: col, zon, bot, v
   AED_REAL :: ta(n_vars+n_vars_ben)
   AED_REAL :: da(n_vars_diag)
   AED_REAL :: fa
!
!-------------------------------------------------------------------------------
!BEGIN
   DO zon=1,n_zones
      aedZones(zon)%z_cc = zero_
      aedZones(zon)%z_cc_diag = zero_

      ta = 0. ; da = 0.
      DO col=1, n_cols
         IF ( active(col) .AND. (zon == zm(col)) ) THEN
            bot = benth_map(col)
            fa = area(col) / aedZones(zon)%z_env%z_area

            ta = ta + (cc(1:n_vars+n_vars_ben,bot) * fa)
            da = da + (cc_diag(:,bot) * fa)
         ENDIF
      ENDDO
      aedZones(zon)%z_cc(1,1:n_vars+n_vars_ben) = ta
      aedZones(zon)%z_cc_diag(1,:) = da
   ENDDO
END SUBROUTINE api_copy_to_zone
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE api_copy_from_zone(theZones, n_zones, heights, x_cc, x_cc_hz, x_diag, x_diag_hz, wlev)
!-------------------------------------------------------------------------------
!ARGUMENTS
   TYPE(api_zone_t),DIMENSION(:),INTENT(in) :: theZones
   INTEGER,INTENT(in) :: n_zones
   AED_REAL,DIMENSION(:),  POINTER,INTENT(in) :: heights
   AED_REAL,DIMENSION(:,:),POINTER,INTENT(inout) :: x_cc
   AED_REAL,DIMENSION(:),  POINTER,INTENT(inout) :: x_cc_hz
   AED_REAL,DIMENSION(:,:),POINTER,INTENT(inout) :: x_diag
   AED_REAL,DIMENSION(:),  POINTER,INTENT(inout) :: x_diag_hz
   INTEGER,INTENT(in) :: wlev
!
!LOCALS
   INTEGER :: col, zon, bot, i, j
   TYPE(aed_variable_t),POINTER :: tvar
!
!-------------------------------------------------------------------------------
!BEGIN
   DO col=1, n_cols
      IF (.NOT. active(col)) CYCLE

      bot = benth_map(col)
      zon = zm(col)

      !# only want the diag vars that have zavg == true
      !    cc_diag(:,bot) = z_cc_diag(:,zon)
      j = 0
      DO i=1,n_aed_vars
         IF ( aed_get_var(i, tvar) ) THEN
            IF ( tvar%diag ) THEN
               j = j + 1
               IF ( tvar%zavg ) x_diag(j,bot) = aedZones(zon)%z_cc_diag(1,j)
            ENDIF
         ENDIF
      ENDDO
   ENDDO
END SUBROUTINE api_copy_from_zone
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#if 0
!###############################################################################
SUBROUTINE STOPIT(message)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CHARACTER(*) :: message
!-------------------------------------------------------------------------------
   PRINT *,message
   STOP "Fatal Error"
END SUBROUTINE STOPIT
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE aed_initialize_zone_benthic(nCols, active, n_aed_vars, cc_diag, benth_map)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in)   :: nCols
   LOGICAL,DIMENSION(:),INTENT(in) :: active
   INTEGER,INTENT(in)   :: n_aed_vars
   AED_REAL,INTENT(out) :: cc_diag(:,:)
   INTEGER,DIMENSION(:),INTENT(in) :: benth_map
!
!LOCALS
   INTEGER :: col, zon, bot
   TYPE (aed_column_t) :: column(n_aed_vars)
!
!-------------------------------------------------------------------------------
!BEGIN
   DO zon=1, n_zones
      z_cc_diag(zon,1,:) = zero_

!CAB      CALL define_column_zone(column, zon, n_aed_vars)

      CALL aed_initialize_benthic(column, 1)
   ENDDO

   CALL copy_from_zone(n_cols, n_aed_vars, cc_diag, active, benth_map)
   !# now copy the diagnostic vars back
!  DO col=1, n_cols
!     IF (.NOT. active(col)) CYCLE

!     bot = benth_map(col)
!     zon = zm(col)

!     cc_diag(:,bot) = z_cc_diag(:,zon)
!  ENDDO
END SUBROUTINE aed_initialize_zone_benthic
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE compute_zone_benthic_fluxes(n_aed_vars)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: n_aed_vars
!
!LOCALS
   INTEGER :: zon, v
   TYPE (aed_column_t) :: column(n_aed_vars)
!
!-------------------------------------------------------------------------------
!BEGIN
   flux_pelz = zero_ ; flux_benz = zero_
!!$OMP DO PRIVATE(zon,column)
   DO zon=1, n_zones
!CAB      CALL define_column_zone(column, zon, n_aed_vars)

      CALL aed_calculate_benthic(column, 1, .TRUE.)
   ENDDO
!!$OMP END DO
END SUBROUTINE compute_zone_benthic_fluxes
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#endif


!===============================================================================
END MODULE fv_aed

!===============================================================================
!
!  * calc_zone areas
!  * do particles
!
!  * loop through columns :
!    + do mobility
!    + do settling
!    + do light
!
!  * if zones :
!      = copy to zones :
!        z_bottom_cell = z_bottom_cel + bottom_cell * (column area / zone_area)
!        z_bot_cell_diag = bot_cell_diag * (column area / zone_area)
!      = compute_zone_benthic
!      = copy_from zones :
!        - copy zone diag to columns bottom cell diags
!
!  * loop through columns :
!    + ch column
!      = do_stress
!      = some stuff
!      = calc_fluxes
!        - calc surface flux
!        - if zones :
!          # add zone pel-flux to column pel-flux
!          # calc benthic fluxes for those models not participating in zones
!        - else (not zones) :
!          # calc_benthic fluxes
!        - divide all pel fluxes by height
!        - do all pelagics
!      = Particles again ?
!      = loop top to bottom applying fluxes
!
!      = if zones :
!        - apply zone fluxes to cells
!      = else :
!        - apply benth fluxes
!
!      = do biodrag
!      = do bioextinction
!
!      = check states
!
!===============================================================================
