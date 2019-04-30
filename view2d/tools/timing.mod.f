      module TIME_CONTROLS

      type TIMING
         integer :: year
         integer :: month
         integer :: day
         integer :: days0
         integer :: cal_day
         integer :: index
	 logical :: active                          ! whether active or not
	 character(len=5) :: mode
         end type TIMING

      end module TIME_CONTROLS
