" Vim syntax file
" Language:	Nexus file format with some reserved words for MrBayes
" Maintainer:	Luis Carvalho <lexcarvalho@hotmail.com>
" Last Change:	12/11/2015 03:00:06 PM (nylander)
" TODO:         Handle keywords separated by colonr. E.g., 'gene:1-3',
"               or 'brlenspr=clock:fossilization'.
"               Handle same word as both Command/Parameter/Option.
"               E.g., 'Partition name = 2: ...', and
"               'Hompart partition=genes ...'.
"               Allow shortest unambiguous math of keywords. E.g.,
"               '[exe]cute'. Possible?
"               

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" Ignore case
syn case ignore

" Public blocks
syn keyword	nexusBlock	begin end endblock
syn keyword	nexusPublic	taxa characters unaligned distances
syn keyword	nexusPublic	sets assumptions codons trees notes

" Main reserved words
syn keyword	nexusBatch	dimensions format matrix translate tree
syn keyword	nexusBatch	taxlabels

" Commands
syn keyword	nexusCommand	about acknowledgments agree alltrees
syn keyword	nexusCommand	assume bandb base basefreqs
syn keyword	nexusCommand	boot bootstrap calibrate cd
syn keyword	nexusCommand	charpartition charset charstat citations
syn keyword	nexusCommand	cleartrees compareref comparetree condense
syn keyword	nexusCommand	constraint constraints contree cstatus
syn keyword	nexusCommand	ctype databreaks defaults del
syn keyword	nexusCommand	delete deroot deroottrees describetrees
syn keyword	nexusCommand	dimensions disclaimer dos dscores
syn keyword	nexusCommand	dset edit exclude exe
syn keyword	nexusCommand	execute export exset factory
syn keyword	nexusCommand	filter format fstatus gammaplot
syn keyword	nexusCommand	generatetrees gettrees help hompart
syn keyword	nexusCommand	hsearch include ingroup jackknife
syn keyword	nexusCommand	lake leave link loadconstr
syn keyword	nexusCommand	log lscores lset manual
syn keyword	nexusCommand	matrix matrixrep mcmc mcmcp
syn keyword	nexusCommand	mprsets nj options outgroup
syn keyword	nexusCommand	pairdiff pairs partition permute
syn keyword	nexusCommand	plot props propset prset
syn keyword	nexusCommand	pscores pset puzzle quit
syn keyword	nexusCommand	randtrees rateset reconstruct report
syn keyword	nexusCommand	restore revfilter reweight root
syn keyword	nexusCommand	roottrees saveassum savedist savetrees
syn keyword	nexusCommand	set showanc showbeagle showcharparts
syn keyword	nexusCommand	showconstr showdist showmatrix showmcmctrees
syn keyword	nexusCommand	showmodel showmoves showparams showratesets
syn keyword	nexusCommand	showtaxparts showtree showtrees showusertrees
syn keyword	nexusCommand	showusertypes sorttrees ss ssp
syn keyword	nexusCommand	stardecomp startvals sump sumss
syn keyword	nexusCommand	sumt surfcheck taxastat taxlabels
syn keyword	nexusCommand	taxpartition taxset time tonexus
syn keyword	nexusCommand	translate tree treedist treeinfo
syn keyword	nexusCommand	treewts tstatus typeset undelete
syn keyword	nexusCommand	unlink upgma usertree usertype
syn keyword	nexusCommand	version weights wts wtset

" Parameters
syn keyword	nexusParameter	aamodel aamodelpr aarevmatpr adams
syn keyword	nexusParameter	addseq allavailable allchains allcomps
syn keyword	nexusParameter	allruns allsitesmean alpha ancstates
syn keyword	nexusParameter	apolist append applyto askmore
syn keyword	nexusParameter	augment autoclose autocomplete autoreplace
syn keyword	nexusParameter	autotune backbone base basefreq
syn keyword	nexusParameter	beagledevice beaglefreq beagleopenmp beagleprecision
syn keyword	nexusParameter	beagleresource beaglescaling beaglesse beaglethreads
syn keyword	nexusParameter	bmbranchrates bmvar bmvarpr breakties
syn keyword	nexusParameter	brlens brlensgeq brlenspr browncorrpr
syn keyword	nexusParameter	brownscalepr bseed burnin burninfrac
syn keyword	nexusParameter	burninss calctreeprobs calwaitpr characters
syn keyword	nexusParameter	charsperline checkfreq checkpoint chglist
syn keyword	nexusParameter	class clockratepr clockvarpr cmcolwid
syn keyword	nexusParameter	cmcstatus cmlabels cmshoweq code
syn keyword	nexusParameter	coding codoncatfreqs collapse conformat
syn keyword	nexusParameter	conlevel consensus constraints contype
syn keyword	nexusParameter	converse convexity correlation coswitchpr
syn keyword	nexusParameter	covarion covswitchpr cppevents cppmultdev
syn keyword	nexusParameter	cppmultdevpr cpprate cppratepr crit
syn keyword	nexusParameter	criterion cutoffpct data datatype
syn keyword	nexusParameter	dcollapse deldupes diag diagnfreq
syn keyword	nexusParameter	diagnstat dir discardfrac displaygeq
syn keyword	nexusParameter	displayout distance dstatus enforce
syn keyword	nexusParameter	equate estfreq excluded exset
syn keyword	nexusParameter	extincionrates extinctionpr extinctionrate extinctionrates
syn keyword	nexusParameter	fd fdclasses fdfile fdonly
syn keyword	nexusParameter	fdtype file filename filename1
syn keyword	nexusParameter	filename2 fossilizationpr fossilizationrate fromprior
syn keyword	nexusParameter	full fvalue gap generatepr
syn keyword	nexusParameter	gibbsfreq growthpr growthrate grpfreq
syn keyword	nexusParameter	hennig homoplasy hpd ibrbranchlens
syn keyword	nexusParameter	ibrvar ibrvarpr igrbranchrates igrvar
syn keyword	nexusParameter	igrvarpr increase indices interleave
syn keyword	nexusParameter	interleaved irreversible keep keepall
syn keyword	nexusParameter	labelnodes le50 linebreaks longfmt
syn keyword	nexusParameter	m3omegapr majrule marglike match
syn keyword	nexusParameter	matchchar matrixinfo maxtrees mcmcdiagn
syn keyword	nexusParameter	minpartfreq minprob missdist missing
syn keyword	nexusParameter	mixedbrchrates mixedvar mixedvarpr monophyly
syn keyword	nexusParameter	mprsets mrbayes mstaxa mulpars
syn keyword	nexusParameter	multrees nbetacat ncat nchains
syn keyword	nexusParameter	nchar negbrlen nexus ngammacat
syn keyword	nexusParameter	ngen nodeagepr noop nowarn
syn keyword	nexusParameter	nowarnings nperts npthreads nreps
syn keyword	nexusParameter	nruns nst nsteps nswaps
syn keyword	nexusParameter	ntax ntrees nucmodel ny98omega1pr
syn keyword	nexusParameter	ny98omega3pr objective omega omegapr
syn keyword	nexusParameter	omegavar ordered ordertaxa ordphylip
syn keyword	nexusParameter	outputname outroot parameter parsmodel
syn keyword	nexusParameter	partition patristic pbf pbfinitburnin
syn keyword	nexusParameter	pbfsampleburnin pbfsamplefreq pbfsampletime percent
syn keyword	nexusParameter	pfile pinv pinvar pinvarpr
syn keyword	nexusParameter	ploidy plot popsize popsizepr
syn keyword	nexusParameter	popvarpr possel power precision
syn keyword	nexusParameter	print printall printbrlens printfreq
syn keyword	nexusParameter	printmax printtofile quitonerror ratecorrpr
syn keyword	nexusParameter	ratemult ratemultiplier ratepr rates
syn keyword	nexusParameter	rclass redirect relburnin removefreq
syn keyword	nexusParameter	replace reprate revmat revmatpr
syn keyword	nexusParameter	revratepr reweight rmat rmatrix
syn keyword	nexusParameter	runidseed samplefreq sampleprob samplestrat
syn keyword	nexusParameter	savebrlens savetrees scientific scorefile
syn keyword	nexusParameter	search seed semistrict seqerror
syn keyword	nexusParameter	shape shapepr showtree showtreeprobs
syn keyword	nexusParameter	siteomega sitepartition siterates smoothing
syn keyword	nexusParameter	sorttrees speciationpr speciationrate speciationrates
syn keyword	nexusParameter	speciespartition start startingtree startingtrees
syn keyword	nexusParameter	startparams starttree statefreq statefreqpr
syn keyword	nexusParameter	status steptoplot stop stoprule
syn keyword	nexusParameter	stopval strict subst summary
syn keyword	nexusParameter	swap swapadjacent swapfreq swapseed
syn keyword	nexusParameter	switchrates symbols symdirihyperpr symmetricbetapr
syn keyword	nexusParameter	table taxa tcompress temp
syn keyword	nexusParameter	theta thetapr tk02branchrates tk02var
syn keyword	nexusParameter	tk02varpr topology topologypr tratio
syn keyword	nexusParameter	tratiopr tree treeagepr treefile
syn keyword	nexusParameter	trees tunefreq typeset unordered
syn keyword	nexusParameter	unordphylip upbound usebeagle usegibbs
syn keyword	nexusParameter	userlevel usetreewts warnreset warntree
syn keyword	nexusParameter	warntsave wts wtset xout

" Options
syn keyword	nexusOptions	4by4 abs adgamma allcompat
syn keyword	nexusOptions	allow altnexus always ambequal
syn keyword	nexusOptions	asis avgstddev barchart beta
syn keyword	nexusOptions	bincode blossum blosum bm
syn keyword	nexusOptions	both brlens calibrated ciliate
syn keyword	nexusOptions	ciliates cladogram clock cluster
syn keyword	nexusOptions	codon consistentwith constant constraints
syn keyword	nexusOptions	continuous cpp cprev cpu
syn keyword	nexusOptions	current custom datatype dayhoff
syn keyword	nexusOptions	default developer diploid dir
syn keyword	nexusOptions	dirichlet dist diversity dna
syn keyword	nexusOptions	doswindows doublet dynamic echinoderm
syn keyword	nexusOptions	empirical equal equalin est
syn keyword	nexusOptions	estimate euplotid exp exponential
syn keyword	nexusOptions	f81 f84 faststep figtree
syn keyword	nexusOptions	fixed fossiltip freqpars furthest
syn keyword	nexusOptions	gamma gammamean gap gpu
syn keyword	nexusOptions	gtr halfcompat haploid hennig
syn keyword	nexusOptions	heuristic hide histogram hky85
syn keyword	nexusOptions	ibr ignore igr infer
syn keyword	nexusOptions	infinity informative interleave internal
syn keyword	nexusOptions	invermt invgamma jc jones
syn keyword	nexusOptions	k2p k3p kmaxmini lg
syn keyword	nexusOptions	like likelihood lnorm logdet
syn keyword	nexusOptions	lognormal lsfit lundberg m10
syn keyword	nexusOptions	m3 macintosh matchchar maxbrlens
syn keyword	nexusOptions	maxmini maxstddev me mean
syn keyword	nexusOptions	median metmt midpoint minbrlen
syn keyword	nexusOptions	missing mixed ml monophyl
syn keyword	nexusOptions	morphology mtmam mtrev mycoplasma
syn keyword	nexusOptions	native nchar neili nexus
syn keyword	nexusOptions	nj nni noabsencesites none
syn keyword	nexusOptions	nopresencesites normal nosingletonabsence nosingletonpresence
syn keyword	nexusOptions	nosingletons ntax ny98 off
syn keyword	nexusOptions	offsetexponential offsetgamma offsetlognormal only
syn keyword	nexusOptions	p paraphyl parsimony perfect
syn keyword	nexusOptions	phylip phylogram poisson polytomy
syn keyword	nexusOptions	prev previous prohibit propinv
syn keyword	nexusOptions	proportional protein purge random
syn keyword	nexusOptions	ratio relaxedclock repeatcnt reset
syn keyword	nexusOptions	restriction rna rtrev scaled
syn keyword	nexusOptions	setabsval sets setzero show
syn keyword	nexusOptions	simple sitespec speciestree spr
syn keyword	nexusOptions	standard strict symdir tabtext
syn keyword	nexusOptions	tajnei tamnei tbr terminal
syn keyword	nexusOptions	text ti tk02 topology
syn keyword	nexusOptions	total tratio truncatednormal tv
syn keyword	nexusOptions	uncertain unconstrained uni uniform
syn keyword	nexusOptions	universal unix unmodified upgma
syn keyword	nexusOptions	upholt user var variable
syn keyword	nexusOptions	vertmt vt wag yeast
syn keyword	nexusOptions	zlinked   

" Categorical
syn keyword	nexusCategorical	yes no all

" Todos
syn keyword	nexusTodo		contained TODO FIXME XXX

" nexusCommentGroup allows adding matches for special things in comments
syn cluster	nexusCommentGroup	contains=nexusTodo

" Catch errors caused by wrong parenthesis and brackets
syn cluster	nexusParenexusGroup	contains=nexusParenexusError,@nexusCommentGroup,nexusCommentStartError,nexusNumber,nexusFloat
syn region	nexusParen		transparent start='(' end=')' contains=ALLBUT,@nexusParenexusGroup,nexusErrInexusBracket
syn match	nexusParenexusError	display "[\])]"
syn match	nexusErrInexusParen	display contained "[\]{}]"
syn region	nexusBracket		transparent start='\[' end='\]' contains=ALLBUT,@nexusParenexusGroup,nexusErrInexusParen
syn match	nexusErrInexusBracket	display contained "[);{}]"

" Integer or floating point number
syn case ignore
syn match	nexusNumbersCom		display transparent "\<\d\|\.\d" contains=nexusNumber,nexusFloat
syn match	nexusNumber		display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
syn match	nexusNumber		display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Floating point number, with dot, optional exponent
syn match	nexusFloat		display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
" Floating point number, starting with a dot, optional exponent
syn match	nexusFloat		display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
" Floating point number, without dot, with exponent
syn match	nexusFloat		display contained "\d\+e[-+]\=\d\+[fl]\=\>"

" Comments
syn region	nexusComment	start="\[" end="\]" contains=@nexusCommentGroup,nexusCommentStartError
syntax match	nexusCommentError	display "\]"
syntax match	nexusCommentStartError	display "\["me=e-1 contained

" Define the default highlighting
if version >= 508 || !exists("did_nexus_syn_inits")
  if version < 508
    let did_nexus_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

	HiLink nexusBlock		Repeat
	HiLink nexusPublic		Structure
	HiLink nexusBatch		Conditional
	HiLink nexusCommand		Statement
	HiLink nexusParameter		Type
	HiLink nexusOptions 		String
	HiLink nexusCategorical		Constant

	HiLink nexusNumber		Number
	HiLink nexusNumber		Number
	HiLink nexusFloat		Float
	HiLink nexusParenexusError	nexusError
	HiLink nexusErrInexusParen	nexusError
	HiLink nexusErrInexusBracket	nexusError
	HiLink nexusCommentError	nexusError
	HiLink nexusCommentStartError	nexusError
	HiLink nexusError		Error
	HiLink nexusCommentStart	nexusComment
	HiLink nexusComment		Comment
	HiLink nexusTodo		Todo

	delcommand HiLink
endif

let b:current_syntax = "nexus"
