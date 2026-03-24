!###############################################################################
!#                                                                             #
!# fv_api_zones.F90                                                            #
!#                                                                             #
!# Interface for FV (Finite Volume) Model to AED modules.                      #
!#   Designed for TUFLOW-FV, released by BMT-WBM:                              #
!#   http://www.tuflow.com/Tuflow%20FV.aspx                                    #
!#                                                                             #
!# This is a support module to allow ability for benthic/sediment zones in     #
!# AED, including zone-averaging                                               #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Developed by :                                                              #
!#     AquaticEcoDynamics (AED) Group                                          #
!#     School of Agriculture and Environment                                   #
!# Copyright 2024-2026 : The University of Western Australia                   #
!#                                                                             #
!# Copyright by the AED-team @ UWA under the GNU Public License - www.gnu.org  #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Created Sep 2024                                                            #
!#                                                                             #
!###############################################################################

!#define DO_MODULE 1

#if DO_MODULE
#include "aed.h"

!###############################################################################
MODULE fv_zones
!-------------------------------------------------------------------------------
   USE aed_common

   USE aed_api
   USE aed_zones

   IMPLICIT NONE

   PRIVATE

   PUBLIC api_set_fv_zones
!  PUBLIC init_zones, calc_zone_areas, copy_to_zone, copy_from_zone
!  PUBLIC compute_zone_benthic_fluxes, aed_initialize_zone_benthic
   PUBLIC n_zones
!  PUBLIC STOPIT
!  PUBLIC zone, zm, flux_pelz, flux_benz

   !#--------------------------------------------------------------------------#
   !# Module Data

   !# Arrays for environmental variables not supplied externally.
   AED_REAL,DIMENSION(:,:,:),ALLOCATABLE,TARGET :: z_cc
   AED_REAL,DIMENSION(:,:),  ALLOCATABLE,TARGET :: z_cc_hz
   AED_REAL,DIMENSION(:,:,:),ALLOCATABLE,TARGET :: z_cc_diag
   AED_REAL,DIMENSION(:,:),  ALLOCATABLE,TARGET :: z_cc_diag_hz

   INTEGER, DIMENSION(:),  ALLOCATABLE        :: zone_count, zm
   INTEGER :: n_zones, n_vars, n_vars_ben, n_vars_diag, n_vars_diag_sheet, n_aed_vars
   INTEGER :: n_cols

!-------------------------------------------------------------------------------

CONTAINS
#endif
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
!     aedZones(zon)%z_env%z_taub = col_taub
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


#if DO_MODULE
!===============================================================================
END MODULE fv_zones
#endif
