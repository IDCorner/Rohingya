** This is Mahalanabis score

*preserve

*sort Upazilla, stab

use RohingyaIDC

*** baseline ID
	mahascore agrimalewagert agrifemwagert  finericepr  coarsericepr ///
	wheatpr coarseflourpr potatopr   ///
	agriproductivity croppedarea irrigatedarea  riverineforest ///
	metalledroad trail permwaterpct  ,  refobs(23) compute_invcovarmat gen(id1)

****Ricardo Baseline
	mahascore   finericepr  coarsericepr ///
	wheatpr potatopr   ///
	agriproductivity croppedarea irrigatedarea  riverineforest ///
	metalledroad trail permwaterpct cycloneshelter ,  refobs(23) compute_invcovarmat gen(idR1)

****Ricardo R2
	mahascore   finericepr  coarsericepr ///
	wheatpr potatopr   ///
	agriproductivity croppedarea irrigatedarea  riverineforest ///
	metalledroad trail permwaterpct cycloneshelter floodcampshelter numfoodwareh densitysqkm ,  refobs(23) compute_invcovarmat gen(idR2)

	
