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
!# (C) The University of Western Australia                                     #
!#                                                                             #
!# Copyright by the AED-team @ UWA under the GNU Public License - www.gnu.org  #
!#                                                                             #
!#   -----------------------------------------------------------------------   #
!#                                                                             #
!# Created Sep 2024                                                            #
!#                                                                             #
!###############################################################################

#include "aed.h"

!###############################################################################
<<<<<<< HEAD
MODULE fv_api_zones
=======
!MODULE fv_api_zones
MODULE fv_zones
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
!-------------------------------------------------------------------------------
   USE aed_common

   USE aed_api
   USE aed_zones

   IMPLICIT NONE

   PRIVATE

<<<<<<< HEAD
   PUBLIC init_zones, calc_zone_areas, copy_to_zone, copy_from_zone
   PUBLIC compute_zone_benthic_fluxes, aed_initialize_zone_benthic
   PUBLIC n_zones
   PUBLIC STOPIT
   PUBLIC zone, zm, flux_pelz, flux_benz
=======
   PUBLIC api_init_zones
!  PUBLIC compute_zone_benthic_fluxes, aed_initialize_zone_benthic
   PUBLIC n_zones
!  PUBLIC zone, zm, flux_pelz, flux_benz
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142

   !#--------------------------------------------------------------------------#
   !# Module Data

   !# Arrays for environmental variables not supplied externally.
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone

   AED_REAL,DIMENSION(:,:,:),ALLOCATABLE,TARGET :: zone_cc
   AED_REAL,DIMENSION(:,:),ALLOCATABLE,TARGET   :: zone_cc_hz
   AED_REAL,DIMENSION(:,:,:),ALLOCATABLE,TARGET :: zone_cc_diag
   AED_REAL,DIMENSION(:,:),ALLOCATABLE,TARGET   :: zone_cc_diag_hz

<<<<<<< HEAD
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_area
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_temp
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_salt
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_rho
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_height
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_extc
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_tss
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_ss1,zone_ss2,zone_ss3,zone_ss4
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_par
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_nir
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_uva
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_uvb
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_wind
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_rain
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_rainloss
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_air_temp
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_air_pres
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_humidity
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_bathy
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_I_0
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_longwave
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_colnums
   AED_REAL,DIMENSION(:),  ALLOCATABLE,TARGET :: zone_coldepth
   INTEGER, DIMENSION(:),  ALLOCATABLE        :: zone_count, zm

   AED_REAL,DIMENSION(:,:),ALLOCATABLE,TARGET :: flux_pelz
   AED_REAL,DIMENSION(:,:),ALLOCATABLE,TARGET :: flux_benz

   AED_REAL,TARGET :: zone_taub

   INTEGER :: n_zones, nwq_var, nben_var, ndiag_var, n_diag_hz
=======
   INTEGER, DIMENSION(:),  ALLOCATABLE        :: zone_count, zm

   INTEGER,DIMENSION(:),POINTER :: surf_map  => null()
   INTEGER,DIMENSION(:),POINTER :: benth_map => null()

   LOGICAL,DIMENSION(:), POINTER :: active   => null()
   AED_REAL,DIMENSION(:),POINTER :: area     => null()
!******
   AED_REAL,DIMENSION(:,:),POINTER :: rad       => null()
   AED_REAL,DIMENSION(:),  POINTER :: temp      => null()
   AED_REAL,DIMENSION(:),  POINTER :: salt      => null()
   AED_REAL,DIMENSION(:),  POINTER :: rho       => null()
   AED_REAL,DIMENSION(:),  POINTER :: nuh       => null()
   AED_REAL,DIMENSION(:),  POINTER :: h         => null()
   AED_REAL,DIMENSION(:),  POINTER :: depth     => null()
   AED_REAL,DIMENSION(:),  POINTER :: dz        => null()
   AED_REAL,DIMENSION(:),  POINTER :: extc      => null()
   AED_REAL,DIMENSION(:),  POINTER :: tss       => null()
   AED_REAL,DIMENSION(:),  POINTER :: ss1       => null()
   AED_REAL,DIMENSION(:),  POINTER :: ss2       => null()
   AED_REAL,DIMENSION(:),  POINTER :: ss3       => null()
   AED_REAL,DIMENSION(:),  POINTER :: ss4       => null()
   AED_REAL,DIMENSION(:),  POINTER :: bio_drag  => null()
   AED_REAL,DIMENSION(:),  POINTER :: I_0       => null()
   AED_REAL,DIMENSION(:),  POINTER :: wnd       => null()
   AED_REAL,DIMENSION(:),  POINTER :: air_temp  => null()
   AED_REAL,DIMENSION(:),  POINTER :: air_pres  => null()
   AED_REAL,DIMENSION(:),  POINTER :: rain      => null()
   AED_REAL,DIMENSION(:),  POINTER :: humidity  => null()
   AED_REAL,DIMENSION(:),  POINTER :: longwave  => null()
!  AED_REAL,DIMENSION(:),  POINTER :: area      => null()
   AED_REAL,DIMENSION(:),  POINTER :: bathy     => null()
   AED_REAL,DIMENSION(:),  POINTER :: shadefrac => null()
   AED_REAL,DIMENSION(:),  POINTER :: rainloss  => null()
   AED_REAL,DIMENSION(:),  POINTER :: ustar_bed => null()
   AED_REAL,DIMENSION(:),  POINTER :: wv_uorb   => null()
   AED_REAL,DIMENSION(:),  POINTER :: wv_t      => null()
   AED_REAL,DIMENSION(:),  POINTER :: vvel      => null()    !# vertical velocity
   AED_REAL,DIMENSION(:),  POINTER :: cvel      => null()    !# cell velocity

   AED_REAL,DIMENSION(:),  POINTER :: nir
   AED_REAL,DIMENSION(:),  POINTER :: par
   AED_REAL,DIMENSION(:),  POINTER :: uva
   AED_REAL,DIMENSION(:),  POINTER :: uvb

!******

   AED_REAL :: col_taub = 0.


   AED_REAL,DIMENSION(:,:),ALLOCATABLE,TARGET :: flux_pelz
   AED_REAL,DIMENSION(:,:),ALLOCATABLE,TARGET :: flux_benz

!  AED_REAL,TARGET :: zone_taub

   INTEGER :: n_zones, n_cols
   INTEGER :: nwq_var, nben_var, ndiag_var, n_diag_hz, n_aed_vars
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142

!#####################################################

CONTAINS
!===============================================================================


!###############################################################################
<<<<<<< HEAD
SUBROUTINE init_zones(nCols, mat_id, avg, n_vars, n_vars_ben, n_vars_diag)
=======
SUBROUTINE api_init_zones(nCols, mat_id, avg, n_vars, n_vars_ben, n_vars_diag, n_vars_diag_hz)
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in) :: nCols
   INTEGER,DIMENSION(:,:),INTENT(in) :: mat_id
   LOGICAL,INTENT(in) :: avg
<<<<<<< HEAD
   INTEGER,INTENT(in) :: n_vars, n_vars_ben, n_vars_diag
=======
   INTEGER,INTENT(in) :: n_vars, n_vars_ben, n_vars_diag, n_vars_diag_hz
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
!
!LOCALS
   INTEGER :: i,j, cType, nTypes
   INTEGER :: col, zon
   INTEGER,DIMENSION(:),ALLOCATABLE :: mat_t
!
<<<<<<< HEAD
!-------------------------------------------------------------------------------
!BEGIN
=======
   PROCEDURE(copy_to_zone_t),POINTER    :: copy_to
   PROCEDURE(copy_from_zone_t),POINTER  :: copy_from
   PROCEDURE(calc_zone_areas_t),POINTER :: calc_areas
!
!-------------------------------------------------------------------------------
!BEGIN
   n_cols = nCols
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
   ALLOCATE(mat_t(nCols)) ; ALLOCATE(zm(nCols))
   mat_t = 0   ; zm = 1 ; zon = 1
   !# The new form of zones
   cType = mat_id(1,1) ; nTypes = 1 ; mat_t(nTypes) = mat_id(1,1)
   DO col=1, ubound(mat_id,2)
      !# use the bottom index to fill the array
!     print*, mat_id(1,col)
      IF ( cType /= mat_id(1,col) ) THEN
         DO zon=1,nTypes
            IF ( mat_t(zon) .eq. mat_id(1,col) ) THEN
               cType = mat_id(1,col)
               EXIT
            ENDIF
         ENDDO
      ENDIF
      IF ( cType /= mat_id(1,col) ) THEN
         nTypes = nTypes + 1
         mat_t(nTypes) = mat_id(1,col)
         cType = mat_id(1,col)
         zon = nTypes
      ENDIF
      zm(col) = zon
   ENDDO
   print*,"        material (benthic) zones (", nTypes, " in total) = ", mat_t(1:nTypes)
   n_zones = nTypes

!  zone_heights => theZones%zheight

   ALLOCATE(zone_cc(n_zones, 1, n_vars+n_vars_ben))   ; zone_cc = zero_
   ALLOCATE(zone_cc_hz(n_zones+1, n_vars+n_vars_ben)) ; zone_cc_hz = zero_

   ALLOCATE(zone_cc_diag(n_zones, 1, ndiag_var))      ; zone_cc_diag = zero_
   ALLOCATE(zone_cc_diag_hz(n_zones+1, n_diag_hz))    ; zone_cc_diag_hz = zero_

   !# This will create n_zones and storage for things like area, temp etc
   CALL aed_init_zones(n_zones, 1, zone_cc, zone_cc_hz, zone_cc_diag, zone_cc_diag_hz)

<<<<<<< HEAD
   DO zon=1,n_zones
      zone(zon) = mat_t(zon)
      zone_colnums(zon) = zon
=======
   copy_to => api_copy_to_zone
   copy_from => api_copy_from_zone
   calc_areas => api_calc_zone_areas
   CALL api_set_zone_funcs(copy_to, copy_from, calc_areas)

   DO zon=1,n_zones
      zone(zon) = mat_t(zon)
      aedZones(zon)%z_colnums = zon
!     aedZones(zon)%z_heights(1) = theZones(zon)%zheight
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
   ENDDO

   ALLOCATE(flux_pelz(n_vars+n_vars_ben, n_zones)) ; flux_pelz = 0.
   ALLOCATE(flux_benz(n_vars+n_vars_ben, n_zones)) ; flux_benz = 0.

<<<<<<< HEAD
   IF ( .NOT. avg ) RETURN
=======
   DEALLOCATE(mat_t)

!  IF ( .NOT. avg ) RETURN
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142

   nwq_var = n_vars
   nben_var = n_vars_ben
   ndiag_var = n_vars_diag
<<<<<<< HEAD
END SUBROUTINE init_zones
=======

   n_aed_vars = n_vars+n_vars_ben+n_vars_diag+n_vars_diag_hz
END SUBROUTINE api_init_zones
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
<<<<<<< HEAD
! SUBROUTINE api_calc_zone_areas(aedZones, n_zones, areas, wlev, surf)
! !-------------------------------------------------------------------------------
! !ARGUMENTS
!    TYPE(api_zone_t),DIMENSION(:),INTENT(inout) :: aedZones
!    INTEGER,INTENT(in) :: n_zones
! !
!    AED_REAL,DIMENSION(:),INTENT(in) :: areas
!    INTEGER,INTENT(in) :: wlev
!    AED_REAL :: surf
!###############################################################################
SUBROUTINE calc_zone_areas(nCols, active, temp, salt, h, z, area, wnd, rho,    &
                           extcoeff, I_0, longwave, nir, par, uva, uvb, tss,   &
                           rain, rainloss,air_temp, humidity, bathy, col_taub, &
                           air_pres)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,              INTENT(in) :: nCols
   LOGICAL, DIMENSION(:),INTENT(in) :: active
   AED_REAL,DIMENSION(:),INTENT(in) :: h, z, area, bathy
   AED_REAL,DIMENSION(:),INTENT(in) :: temp, salt, rho
   AED_REAL,DIMENSION(:),INTENT(in) :: I_0, extcoeff, nir, par, uva, uvb
   AED_REAL,DIMENSION(:),INTENT(in) :: wnd, rain, longwave, rainloss, air_temp, humidity
   AED_REAL,DIMENSION(:),INTENT(in) :: tss, air_pres
   AED_REAL :: col_taub
=======
SUBROUTINE api_calc_zone_areas(aedZones, n_zones, areas, heights, wlev)
!-------------------------------------------------------------------------------
!ARGUMENTS
   TYPE(api_zone_t),DIMENSION(:),INTENT(inout) :: aedZones
   INTEGER,INTENT(in) :: n_zones
   AED_REAL,DIMENSION(:),INTENT(in) :: areas
   AED_REAL,DIMENSION(:),POINTER,INTENT(in) :: heights
   INTEGER,INTENT(in) :: wlev
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
!
!LOCALS
   INTEGER :: col, zon
   INTEGER :: dbg = 0 !29
!
!-------------------------------------------------------------------------------
!BEGIN

<<<<<<< HEAD
   zone_area = zero_
   zone_temp = zero_
   zone_salt = zero_
   zone_rho  = zero_
   zone_height = zero_
   zone_extc   = zero_
   zone_tss = zero_
   zone_ss1 = zero_
   zone_ss2 = zero_
   zone_ss3 = zero_
   zone_ss4 = zero_
   zone_nir = zero_
   zone_par = zero_
   zone_uva = zero_
   zone_uvb = zero_
   zone_wind = zero_
   zone_rain = zero_
   zone_rainloss = zero_
   zone_air_temp = zero_
   zone_air_pres = zero_
   zone_humidity = zero_
   zone_bathy = zero_
   zone_I_0 = zero_
   zone_longwave = zero_
   zone_taub = col_taub
   zone_coldepth = one_
   zone_count = 0

   ! loop thru all columns in the mesh
   DO col=1, nCols
=======
   DO zon=1, n_zones
      aedZones(zon)%z_area = zero_
      aedZones(zon)%z_temp = zero_
      aedZones(zon)%z_salt = zero_
      aedZones(zon)%z_rho  = zero_
      aedZones(zon)%z_heights = zero_
      aedZones(zon)%z_extc   = zero_
      aedZones(zon)%z_tss = zero_
      aedZones(zon)%z_ss1 = zero_
      aedZones(zon)%z_ss2 = zero_
      aedZones(zon)%z_ss3 = zero_
      aedZones(zon)%z_ss4 = zero_
      aedZones(zon)%z_nir = zero_
      aedZones(zon)%z_par = zero_
      aedZones(zon)%z_uva = zero_
      aedZones(zon)%z_uvb = zero_
      aedZones(zon)%z_wind = zero_
      aedZones(zon)%z_rain = zero_
      aedZones(zon)%z_rainloss = zero_
      aedZones(zon)%z_air_temp = zero_
      aedZones(zon)%z_air_pres = zero_
      aedZones(zon)%z_humidity = zero_
      aedZones(zon)%z_bathy = zero_
      aedZones(zon)%z_I_0 = zero_
      aedZones(zon)%z_longwave = zero_
      aedZones(zon)%z_layer_stress = col_taub
      aedZones(zon)%z_coldepth = one_
   ENDDO
   zone_count = 0

   ! loop thru all columns in the mesh
   DO col=1, n_cols
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
      ! zone number of this column
      zon = zm(col)

    ! if (zone(zon) == 11) &
    ! print*,"ZoneIDX ",zon," zone = ",zone(zon)," Col ",col," area col ",area(col),"TEMP ",temp(col)," is ",active(col)
      IF (.NOT. active(col)) CYCLE

      ! cumulate column into relevant zone vars
<<<<<<< HEAD
      zone_area(zon)     = zone_area(zon) + area(col)

      zone_temp(zon)     = zone_temp(zon) + temp(col)
      zone_salt(zon)     = zone_salt(zon) + salt(col)
      zone_rho(zon)      = zone_rho(zon) + rho(col)
      zone_height(zon)   = zone_height(zon) + h(col)
      zone_coldepth(zon) = zone_coldepth(zon) + z(col)
      zone_extc(zon)     = zone_extc(zon) + extcoeff(col)
      zone_tss(zon)      = zone_tss(zon) + tss(col)
      zone_ss1(zon)      = zone_ss1(zon) + tss(col)  !   For FV API 2.0 (To be connected to sed_conc)
      zone_ss2(zon)      = zone_ss2(zon) + tss(col)  !   For FV API 2.0 (To be connected to sed_conc)
      zone_ss3(zon)      = zone_ss3(zon) + tss(col)  !   For FV API 2.0 (To be connected to sed_conc)
      zone_ss4(zon)      = zone_ss4(zon) + tss(col)  !   For FV API 2.0 (To be connected to sed_conc)
      zone_nir(zon)      = zone_nir(zon) + nir(col)  !   For FV API 2.0 (To be connected to light)
      zone_par(zon)      = zone_par(zon) + par(col)  !   For FV API 2.0 (To be connected to light)
      zone_uva(zon)      = zone_uva(zon) + uva(col)  !   For FV API 2.0 (To be connected to light)
      zone_uvb(zon)      = zone_uvb(zon) + uvb(col)  !   For FV API 2.0 (To be connected to light)
      zone_wind(zon)     = zone_wind(zon) + wnd(col)
      zone_rain(zon)     = zone_rain(zon) + rain(col)
      zone_rainloss(zon) = zone_rainloss(zon) + rainloss(col)
      zone_air_temp(zon) = zone_air_temp(zon) + air_temp(col)
      zone_air_pres(zon) = zone_air_pres(zon) + air_pres(col)
      zone_humidity(zon) = zone_humidity(zon) + humidity(col)
      zone_bathy(zon)    = zone_bathy(zon) + bathy(col)
      zone_I_0(zon)      = zone_I_0(zon) + I_0(col)
      zone_longwave(zon) = zone_longwave(zon) + longwave(col)
     !zone_taub(zon)     = zone_taub(zon) + col_taub
=======
      aedZones(zon)%z_area     = aedZones(zon)%z_area + area(col)

      aedZones(zon)%z_temp     = aedZones(zon)%z_temp + temp(col)
      aedZones(zon)%z_salt     = aedZones(zon)%z_salt + salt(col)
      aedZones(zon)%z_rho      = aedZones(zon)%z_rho + rho(col)
      aedZones(zon)%z_heights  = aedZones(zon)%z_heights + h(col)
      aedZones(zon)%z_coldepth = aedZones(zon)%z_coldepth + depth(col)
      aedZones(zon)%z_extc     = aedZones(zon)%z_extc + extc(col)
      aedZones(zon)%z_tss      = aedZones(zon)%z_tss + tss(col)
      aedZones(zon)%z_ss1      = aedZones(zon)%z_ss1 + tss(col)  !   For FV API 2.0 (To be connected to sed_conc)
      aedZones(zon)%z_ss2      = aedZones(zon)%z_ss2 + tss(col)  !   For FV API 2.0 (To be connected to sed_conc)
      aedZones(zon)%z_ss3      = aedZones(zon)%z_ss3 + tss(col)  !   For FV API 2.0 (To be connected to sed_conc)
      aedZones(zon)%z_ss4      = aedZones(zon)%z_ss4 + tss(col)  !   For FV API 2.0 (To be connected to sed_conc)
      aedZones(zon)%z_nir      = aedZones(zon)%z_nir + nir(col)  !   For FV API 2.0 (To be connected to light)
      aedZones(zon)%z_par      = aedZones(zon)%z_par + par(col)  !   For FV API 2.0 (To be connected to light)
      aedZones(zon)%z_uva      = aedZones(zon)%z_uva + uva(col)  !   For FV API 2.0 (To be connected to light)
      aedZones(zon)%z_uvb      = aedZones(zon)%z_uvb + uvb(col)  !   For FV API 2.0 (To be connected to light)
      aedZones(zon)%z_wind     = aedZones(zon)%z_wind + wnd(col)
      aedZones(zon)%z_rain     = aedZones(zon)%z_rain + rain(col)
      aedZones(zon)%z_rainloss = aedZones(zon)%z_rainloss + rainloss(col)
      aedZones(zon)%z_air_temp = aedZones(zon)%z_air_temp + air_temp(col)
      aedZones(zon)%z_air_pres = aedZones(zon)%z_air_pres + air_pres(col)
      aedZones(zon)%z_humidity = aedZones(zon)%z_humidity + humidity(col)
      aedZones(zon)%z_bathy    = aedZones(zon)%z_bathy + bathy(col)
      aedZones(zon)%z_I_0      = aedZones(zon)%z_I_0 + I_0(col)
      aedZones(zon)%z_longwave = aedZones(zon)%z_longwave + longwave(col)
     !aedZones(zon)%z_taub     = aedZones(zon)%z_taub + col_taub
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142

     ! increment column count
      zone_count(zon) = zone_count(zon) + 1
   ENDDO

   ! finalise the average zone environment values (divide sum by count)
                                                 IF (dbg) print *,'     zone_count: ',zone_count(dbg)
<<<<<<< HEAD
   zone_bathy    =    zone_bathy / zone_count ;  IF (dbg) print *,'     zone_bathy: ',zone_bathy(dbg)
   zone_coldepth = zone_coldepth / zone_count ;  IF (dbg) print *,'     zone_coldepth: ',zone_coldepth(dbg)
  !zone_height   =   zone_height / zone_count    !MH this seems to be missing so just cumulating
   zone_I_0      =      zone_I_0 / zone_count ;  IF (dbg) print *,'     zone_I_0: ',zone_I_0(dbg)
   zone_wind     =     zone_wind / zone_count ;  IF (dbg) print *,'     zone_wind: ',zone_wind(dbg)
   zone_rain     =     zone_rain / zone_count ;  IF (dbg) print *,'     zone_rain: ',zone_rain(dbg)
   zone_rainloss = zone_rainloss / zone_count ;  IF (dbg) print *,'     zone_rainloss: ',zone_rainloss(dbg)
   zone_air_temp = zone_air_temp / zone_count ;  IF (dbg) print *,'     zone_air_temp: ',zone_air_temp(dbg)
   zone_air_pres = zone_air_pres / zone_count ;  IF (dbg) print *,'     zone_air_pres: ',zone_air_pres(dbg)
   zone_humidity = zone_humidity / zone_count ;  IF (dbg) print *,'     zone_humidity: ',zone_humidity(dbg)
   zone_longwave = zone_longwave / zone_count ;  IF (dbg) print *,'     zone_longwave: ',zone_longwave(dbg)
   zone_temp     =     zone_temp / zone_count ;  IF (dbg) print *,'     zone_temp: ',zone_temp(dbg)
   zone_salt     =     zone_salt / zone_count ;  IF (dbg) print *,'     zone_salt: ',zone_salt(dbg)
   zone_rho      =      zone_rho / zone_count ;  IF (dbg) print *,'     zone_rho: ',zone_rho(dbg)
   zone_extc     =     zone_extc / zone_count ;  IF (dbg) print *,'     zone_extc: ',zone_extc(dbg)
  !zone_taub     =     zone_taub / zone_count   !MH also seems to be missing but NOT cumulating
   zone_tss      =      zone_tss / zone_count ;  IF (dbg) print *,'     zone_tss: ',zone_tss(dbg)
   zone_nir      =      zone_nir / zone_count ;  IF (dbg) print *,'     zone_nir: ',zone_nir(dbg)
   zone_par      =      zone_par / zone_count ;  IF (dbg) print *,'     zone_par: ',zone_par(dbg)
   zone_uva      =      zone_uva / zone_count ;  IF (dbg) print *,'     zone_uva: ',zone_uva(dbg)
   zone_uvb      =      zone_uvb / zone_count ;  IF (dbg) print *,'     zone_uvb: ',zone_uvb(dbg)


   ! clean empty zones   !MH THERE WILL BE A DIVEDE BY ZERO BEFORE THIS, ABOVE.
   do zon=1,n_zones
     !print *,"zoneidx ",zon," zone ",zone(zon)," count ",zone_count(zon)
      if (zone_count(zon) == 0) then
         zone_area(zon)     = 0.0
         zone_temp(zon)     = 0.0
         zone_salt(zon)     = 0.0
         zone_rho(zon)      = 0.0
         zone_height(zon)   = 0.0
         zone_extc(zon)     = 0.0
         zone_tss(zon)      = 0.0
         zone_ss1(zon)      = 0.0
         zone_ss2(zon)      = 0.0
         zone_ss3(zon)      = 0.0
         zone_ss4(zon)      = 0.0
         zone_nir(zon)      = 0.0
         zone_par(zon)      = 0.0
         zone_uva(zon)      = 0.0
         zone_uvb(zon)      = 0.0
         zone_wind(zon)     = 0.0
         zone_rain(zon)     = 0.0
         zone_rainloss(zon) = 0.0
         zone_air_temp(zon) = 0.0
         zone_air_pres(zon) = 0.0
         zone_humidity(zon) = 0.0
         zone_bathy(zon)    = 0.0
         zone_I_0(zon)      = 0.0
         zone_longwave(zon) = 0.0
        !zone_taub(zon)     = 0.0
      endif

   end do
END SUBROUTINE calc_zone_areas
=======
   WHERE (zone_count /= 0.)
      aedZones(zon)%z_bathy    =    aedZones(zon)%z_bathy / zone_count ;!  IF (dbg) print *,'    zone_bathy: ',zone_bathy(dbg)
      aedZones(zon)%z_coldepth = aedZones(zon)%z_coldepth / zone_count ;!  IF (dbg) print *,'    zone_coldepth: ',zone_coldepth(dbg)
  !   aedZones(zon)%z_heights  =  aedZones(zon)%z_heights / zone_count    !MH this seems to be missing so just cumulating
      aedZones(zon)%z_I_0      =      aedZones(zon)%z_I_0 / zone_count ;!  IF (dbg) print *,'    zone_I_0: ',zone_I_0(dbg)
      aedZones(zon)%z_wind     =     aedZones(zon)%z_wind / zone_count ;!  IF (dbg) print *,'    zone_wind: ',zone_wind(dbg)
      aedZones(zon)%z_rain     =     aedZones(zon)%z_rain / zone_count ;!  IF (dbg) print *,'    zone_rain: ',zone_rain(dbg)
      aedZones(zon)%z_rainloss = aedZones(zon)%z_rainloss / zone_count ;!  IF (dbg) print *,'    zone_rainloss: ',zone_rainloss(dbg)
      aedZones(zon)%z_air_temp = aedZones(zon)%z_air_temp / zone_count ;!  IF (dbg) print *,'    zone_air_temp: ',zone_air_temp(dbg)
      aedZones(zon)%z_air_pres = aedZones(zon)%z_air_pres / zone_count ;!  IF (dbg) print *,'    zone_air_pres: ',zone_air_pres(dbg)
      aedZones(zon)%z_humidity = aedZones(zon)%z_humidity / zone_count ;!  IF (dbg) print *,'    zone_humidity: ',zone_humidity(dbg)
      aedZones(zon)%z_longwave = aedZones(zon)%z_longwave / zone_count ;!  IF (dbg) print *,'    zone_longwave: ',zone_longwave(dbg)
      aedZones(zon)%z_temp     =     aedZones(zon)%z_temp / zone_count ;!  IF (dbg) print *,'    zone_temp: ',zone_temp(dbg)
      aedZones(zon)%z_salt     =     aedZones(zon)%z_salt / zone_count ;!  IF (dbg) print *,'    zone_salt: ',zone_salt(dbg)
      aedZones(zon)%z_rho      =      aedZones(zon)%z_rho / zone_count ;!  IF (dbg) print *,'    zone_rho: ',zone_rho(dbg)
      aedZones(zon)%z_extc     =     aedZones(zon)%z_extc / zone_count ;!  IF (dbg) print *,'    zone_extc: ',zone_extc(dbg)
  !   aedZones(zon)%z_taub     =     aedZones(zon)%z_taub / zone_count   !MH also seems to be missing but NOT cumulating
      aedZones(zon)%z_tss      =      aedZones(zon)%z_tss / zone_count ;!  IF (dbg) print *,'    zone_tss: ',zone_tss(dbg)
      aedZones(zon)%z_nir      =      aedZones(zon)%z_nir / zone_count ;!  IF (dbg) print *,'    zone_nir: ',zone_nir(dbg)
      aedZones(zon)%z_par      =      aedZones(zon)%z_par / zone_count ;!  IF (dbg) print *,'    zone_par: ',zone_par(dbg)
      aedZones(zon)%z_uva      =      aedZones(zon)%z_uva / zone_count ;!  IF (dbg) print *,'    zone_uva: ',zone_uva(dbg)
      aedZones(zon)%z_uvb      =      aedZones(zon)%z_uvb / zone_count ;!  IF (dbg) print *,'    zone_uvb: ',zone_uvb(dbg)
   ELSEWHERE
      aedZones(zon)%z_area     = 0.0
      aedZones(zon)%z_temp     = 0.0
      aedZones(zon)%z_salt     = 0.0
      aedZones(zon)%z_rho      = 0.0
      aedZones(zon)%z_heights  = 0.0
      aedZones(zon)%z_extc     = 0.0
      aedZones(zon)%z_tss      = 0.0
      aedZones(zon)%z_ss1      = 0.0
      aedZones(zon)%z_ss2      = 0.0
      aedZones(zon)%z_ss3      = 0.0
      aedZones(zon)%z_ss4      = 0.0
      aedZones(zon)%z_nir      = 0.0
      aedZones(zon)%z_par      = 0.0
      aedZones(zon)%z_uva      = 0.0
      aedZones(zon)%z_uvb      = 0.0
      aedZones(zon)%z_wind     = 0.0
      aedZones(zon)%z_rain     = 0.0
      aedZones(zon)%z_rainloss = 0.0
      aedZones(zon)%z_air_temp = 0.0
      aedZones(zon)%z_air_pres = 0.0
      aedZones(zon)%z_humidity = 0.0
      aedZones(zon)%z_bathy    = 0.0
      aedZones(zon)%z_I_0      = 0.0
      aedZones(zon)%z_longwave = 0.0
  !   aedZones(zon)%z_taub     = 0.0
   ENDWHERE

END SUBROUTINE api_calc_zone_areas
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
<<<<<<< HEAD
! SUBROUTINE api_copy_to_zone(aedZones, n_zones, x_cc, x_cc_hz, x_diag, x_diag_hz, wlev)
! !-------------------------------------------------------------------------------
! !ARGUMENTS
!    TYPE(api_zone_t),DIMENSION(:),INTENT(inout) :: aedZones
!    INTEGER,INTENT(in) :: n_zones
! !
!    AED_REAL,DIMENSION(:,:),INTENT(in) :: x_cc
!    AED_REAL,DIMENSION(:),INTENT(in) :: x_cc_hz
!    AED_REAL,DIMENSION(:,:),INTENT(in) :: x_diag
!    AED_REAL,DIMENSION(:),INTENT(in) :: x_diag_hz
!    INTEGER,INTENT(in) :: wlev
!###############################################################################
SUBROUTINE copy_to_zone(nCols, cc, cc_diag, area, active, benth_map)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in)  :: nCols
   AED_REAL,INTENT(in) :: cc(:,:)       !# (n_vars, n_layers)
   AED_REAL,INTENT(in) :: cc_diag(:,:)  !# (n_vars, n_layers)
   AED_REAL,DIMENSION(:),INTENT(in) :: area
   LOGICAL,DIMENSION(:), INTENT(in) :: active
   INTEGER,DIMENSION(:), INTENT(in) :: benth_map
=======
SUBROUTINE api_copy_to_zone(aedZones, n_zones, heights, x_cc, x_cc_hz, x_cc_diag, x_cc_diag_hz, wlev)
!-------------------------------------------------------------------------------
!ARGUMENTS
   TYPE(api_zone_t),DIMENSION(:),INTENT(inout) :: aedZones
   INTEGER,INTENT(in) :: n_zones
!
   AED_REAL,DIMENSION(:),  POINTER,INTENT(in) :: heights
   AED_REAL,DIMENSION(:,:),POINTER,INTENT(in) :: x_cc
   AED_REAL,DIMENSION(:),  POINTER,INTENT(in) :: x_cc_hz
   AED_REAL,DIMENSION(:,:),POINTER,INTENT(in) :: x_cc_diag
   AED_REAL,DIMENSION(:),  POINTER,INTENT(in) :: x_cc_diag_hz
   INTEGER,INTENT(in) :: wlev
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
!
!LOCALS
   INTEGER :: col, zon, bot, v
   AED_REAL :: ta(nwq_var+nben_var)
   AED_REAL :: da(ndiag_var)
   AED_REAL :: fa
!
!-------------------------------------------------------------------------------
!BEGIN
<<<<<<< HEAD
   zone_cc = zero_
   zone_cc_diag = zero_

   DO zon=1,n_zones
      ta = 0. ; da = 0.
      DO col=1, nCols
         IF ( active(col) .AND. (zon == zm(col)) ) THEN
            bot = benth_map(col)
            fa = area(col) / zone_area(zon)

            ta = ta + (cc(1:nwq_var+nben_var,bot) * fa)
            da = da + (cc_diag(:,bot) * fa)
         ENDIF
      ENDDO
      zone_cc(zon,1,1:nwq_var+nben_var) = ta
      zone_cc_diag(zon,1,:) = da
   ENDDO
END SUBROUTINE copy_to_zone
=======
   DO zon=1,n_zones
      aedZones(zon)%z_cc = zero_
      aedZones(zon)%z_cc_diag = zero_

      ta = 0. ; da = 0.
      DO col=1, n_cols
         IF ( active(col) .AND. (zon == zm(col)) ) THEN
            bot = benth_map(col)
            fa = area(col) / aedZones(zon)%z_area(bot)

            ta = ta + (x_cc(1:nwq_var+nben_var,bot) * fa)
            da = da + (x_cc_diag(:,bot) * fa)
         ENDIF
      ENDDO
      aedZones(zon)%z_cc(1,1:nwq_var+nben_var) = ta
      aedZones(zon)%z_cc_diag(1,:) = da
   ENDDO
END SUBROUTINE api_copy_to_zone
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
<<<<<<< HEAD
! SUBROUTINE api_copy_from_zone(aedZones, n_zones, x_cc, x_cc_hz, x_diag, x_diag_hz, wlev)
! !-------------------------------------------------------------------------------
! !ARGUMENTS
!    TYPE(api_zone_t),DIMENSION(:),INTENT(in) :: aedZones
!    INTEGER,INTENT(in) :: n_zones
! !
!    AED_REAL,DIMENSION(:,:),INTENT(inout) :: x_cc
!    AED_REAL,DIMENSION(:),INTENT(inout) :: x_cc_hz
!    AED_REAL,DIMENSION(:,:),INTENT(inout) :: x_diag
!    AED_REAL,DIMENSION(:),INTENT(inout) :: x_diag_hz
!    INTEGER,INTENT(in) :: wlev
!###############################################################################
! Copies the (sheet, diagnostic) variables from the zones to the main data block
SUBROUTINE copy_from_zone(nCols, n_aed_vars, cc_diag, active, benth_map)
!-------------------------------------------------------------------------------
!ARGUMENTS
   INTEGER,INTENT(in)     :: nCols, n_aed_vars
   AED_REAL,INTENT(out)   :: cc_diag(:,:)  !# (n_vars, n_layers)
   LOGICAL,DIMENSION(:), INTENT(in) :: active
   INTEGER,DIMENSION(:), INTENT(in) :: benth_map
=======
SUBROUTINE api_copy_from_zone(aedZones, n_zones, heights, x_cc, x_cc_hz, x_cc_diag, x_cc_diag_hz, wlev)
!-------------------------------------------------------------------------------
! Copies the (sheet, diagnostic) variables from the zones to the main data block
!-------------------------------------------------------------------------------
! !ARGUMENTS
   TYPE(api_zone_t),DIMENSION(:),INTENT(in) :: aedZones
   INTEGER,INTENT(in) :: n_zones
!
   AED_REAL,DIMENSION(:),  POINTER,INTENT(in) :: heights
   AED_REAL,DIMENSION(:,:),POINTER,INTENT(inout) :: x_cc
   AED_REAL,DIMENSION(:),  POINTER,INTENT(inout) :: x_cc_hz
   AED_REAL,DIMENSION(:,:),POINTER,INTENT(inout) :: x_cc_diag
   AED_REAL,DIMENSION(:),  POINTER,INTENT(inout) :: x_cc_diag_hz
   INTEGER,INTENT(in) :: wlev
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
!
!LOCALS
   INTEGER :: col, zon, bot, i, j
   TYPE(aed_variable_t),POINTER :: tvar
!
!-------------------------------------------------------------------------------
!BEGIN
<<<<<<< HEAD
   DO col=1, nCols
=======
   DO col=1, n_cols
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
      IF (.NOT. active(col)) CYCLE

      bot = benth_map(col)
      zon = zm(col)

      !# only want the diag vars that have zavg == true
      !    cc_diag(:,bot) = zone_cc_diag(:,zon)
      j = 0
      DO i=1,n_aed_vars
         IF ( aed_get_var(i, tvar) ) THEN
            IF ( tvar%diag ) THEN
               j = j + 1
<<<<<<< HEAD
               IF ( tvar%zavg ) cc_diag(j,bot) = zone_cc_diag(zon,1,j)
=======
               IF ( tvar%zavg ) x_cc_diag(j,bot) = aedZones(zon)%z_cc_diag(1,j)
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
            ENDIF
         ENDIF
      ENDDO
   ENDDO
<<<<<<< HEAD
END SUBROUTINE copy_from_zone
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!###############################################################################
SUBROUTINE STOPIT(message)
!-------------------------------------------------------------------------------
!ARGUMENTS
   CHARACTER(*) :: message
!-------------------------------------------------------------------------------
   PRINT *,message
   STOP "Fatal Error"
END SUBROUTINE STOPIT
=======
END SUBROUTINE api_copy_from_zone
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#if 0

!###############################################################################
SUBROUTINE define_column_zone(column, zon, n_aed_vars)!, n_vars)
!-------------------------------------------------------------------------------
!ARGUMENTS
   TYPE (aed_column_t), INTENT(inout) :: column(:)
   INTEGER, INTENT(in) :: zon, n_aed_vars!, n_vars
!
!LOCALS
   INTEGER :: av, i !, top, bot
   INTEGER :: v, d, sv, sd, ev
   TYPE(aed_variable_t),POINTER :: tvar
!
!-------------------------------------------------------------------------------
!BEGIN
   v = 0 ; d = 0; sv = 0; sd = 0 ; ev = 0
   DO av=1,n_aed_vars

      IF ( .NOT. aed_get_var(av, tvar) ) STOP "Error getting variable info"

      IF ( tvar%extern ) THEN !# global variable
         ev = ev + 1
         SELECT CASE (tvar%name)
            CASE ( 'temperature' ) ; column(av)%cell => zone_temp(zon:zon)
            CASE ( 'salinity' )    ; column(av)%cell => zone_salt(zon:zon)
            CASE ( 'density' )     ; column(av)%cell => zone_rho(zon:zon)
            CASE ( 'layer_ht' )    ; column(av)%cell => zone_height(zon:zon)
            CASE ( 'layer_area' )  ; column(av)%cell_sheet => zone_area(zon)
            CASE ( 'rain' )        ; column(av)%cell_sheet => zone_rain(zon)
            CASE ( 'rainloss' )    ; column(av)%cell_sheet => zone_rainloss(zon)
            CASE ( 'material' )    ; column(av)%cell_sheet => zone(zon)
            CASE ( 'bathy' )       ; column(av)%cell_sheet => zone_bathy(zon)
            CASE ( 'extc_coef' )   ; column(av)%cell => zone_extc(zon:zon)
            CASE ( 'tss' )         ; column(av)%cell => zone_tss(zon:zon)
            CASE ( 'ss1' )         ; column(av)%cell => zone_ss1(zon:zon)
            CASE ( 'ss2' )         ; column(av)%cell => zone_ss2(zon:zon)
            CASE ( 'ss3' )         ; column(av)%cell => zone_ss3(zon:zon)
            CASE ( 'ss4' )         ; column(av)%cell => zone_ss4(zon:zon)
            CASE ( 'cell_vel' )    ; column(av)%cell => null() ! zone_cvel
            CASE ( 'nir' )         ; column(av)%cell => zone_nir(zon:zon)
            CASE ( 'par' )         ; column(av)%cell => zone_par(zon:zon)
            CASE ( 'uva' )         ; column(av)%cell => zone_uva(zon:zon)
            CASE ( 'uvb' )         ; column(av)%cell => zone_uvb(zon:zon)
            CASE ( 'sed_zone' )    ; column(av)%cell_sheet => zone(zon)
            CASE ( 'wind_speed' )  ; column(av)%cell_sheet => zone_wind(zon)
            CASE ( 'par_sf' )      ; column(av)%cell_sheet => zone_I_0(zon)
            CASE ( 'taub' )        ; column(av)%cell_sheet => zone_taub
            CASE ( 'air_temp' )    ; column(av)%cell_sheet => zone_air_temp(zon)
            CASE ( 'air_pres' )    ; column(av)%cell_sheet => zone_air_pres(zon)
            CASE ( 'humidity' )    ; column(av)%cell_sheet => zone_humidity(zon)
            CASE ( 'longwave' )    ; column(av)%cell_sheet => zone_longwave(zon)
            CASE ( 'col_num' )     ; column(av)%cell_sheet => zone_colnums(zon)
            CASE ( 'col_depth' )   ; column(av)%cell_sheet => zone_coldepth(zon)

            CASE ( 'nearest_active' ) ; column(av)%cell_sheet => null() ! zone_nearest_active(col);
            CASE ( 'nearest_depth' )  ; column(av)%cell_sheet => null() ! zone_nearest_depth(col);
            CASE DEFAULT ; CALL STOPIT("ERROR: external variable : "//trim(tvar%name)//" : not found when setting zone environment")
         END SELECT
      ELSEIF ( tvar%diag ) THEN  !# Diagnostic variable
         d = d + 1
         IF ( tvar%sheet ) THEN
            column(av)%cell_sheet => zone_cc_diag(zon,1,d)
         ELSE
            column(av)%cell => zone_cc_diag(zon:zon,1,d)
         ENDIF
      ELSE    !# state variable
         IF ( tvar%sheet ) THEN
            sv = sv + 1
            IF ( tvar%bot ) THEN
               column(av)%cell_sheet => zone_cc(zon,1,nwq_var+sv)
            ELSEIF ( tvar%top ) THEN
    !          column(av)%cell_sheet => zone_cc(top,1,nwq_var+sv)
            ENDIF
            column(av)%flux_ben => flux_benz(nwq_var+sv, zon)
    !       column(av)%flux_atm => flux_atm(nwq_var+sv)
         ELSE
            v = v + 1
            column(av)%cell => zone_cc(zon:zon,1,v)
            column(av)%flux_pel => flux_pelz(v, zon:zon)
            column(av)%flux_ben => flux_benz(v, zon)
    !       column(av)%flux_atm => flux_atm(v)
         ENDIF
      ENDIF
   ENDDO
END SUBROUTINE define_column_zone
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
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
      zone_cc_diag(zon,1,:) = zero_

      CALL define_column_zone(column, zon, n_aed_vars)

      CALL aed_initialize_benthic(column, 1)
   ENDDO

   CALL copy_from_zone(nCols, n_aed_vars, cc_diag, active, benth_map)
   !# now copy the diagnostic vars back
!  DO col=1, nCols
!     IF (.NOT. active(col)) CYCLE

!     bot = benth_map(col)
!     zon = zm(col)

!     cc_diag(:,bot) = zone_cc_diag(:,zon)
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
      CALL define_column_zone(column, zon, n_aed_vars)

      CALL aed_calculate_benthic(column, 1, .TRUE.)
   ENDDO
!!$OMP END DO
END SUBROUTINE compute_zone_benthic_fluxes
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<<<<<<< HEAD


!===============================================================================
END MODULE fv_api_zones
=======
#endif


!===============================================================================
!END MODULE fv_api_zones
END MODULE fv_zones
>>>>>>> 5575b76ae5b39c41209933b52fa78f78ab1df142
