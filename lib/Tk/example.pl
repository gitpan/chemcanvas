use Tk;
use Tk::ChemCanvas;
use Tk::LabEntry;
use Tk::LabFrame;
use Chemistry::File::SDF;
use Storable;
use Data::Dumper;


if(-e "templates.store")
  {
    %templates=%{retrieve "templates.store"};
  }
$mw=MainWindow->new(-title=>"ChemCanvasDemo");
$buttonTarget=$mw->Button(-text=>"Molecule",-command=>\&switchTarget)->pack(-anchor=>"nw");
$f1=$mw->Frame()->pack(-side=>"top",-anchor=>"nw");
$DrawingFrame=$mw->LabFrame(-label=>"Molecule Canvas")->pack(-side=>"left",-fill=>"both",-expand=>1);
$modeFrame=$f1->LabFrame(-label=>"Modes")->pack(-side=>"left");
$modeFrame->Button(-text=>"Elememts",-command=>[\&setMode,"elements"])->pack(-side =>'left');
$modeFrame->Button(-text=>"Chain",-command=>[\&setMode,"chain"])->pack(-side =>'left');
$modeFrame->Button(-text=>"Bonds",-command=>[\&setMode,"bonds"])->pack(-side =>'left');
$modeFrame->Button(-text=>"Erase",-command=>[\&setMode,"erase"])->pack(-side =>'left');
$modeFrame->Button(-text=>"Template",-command=>[\&setMode,"template"])->pack(-side =>'left');
$modeFrame->Button(-text=>"UNDO",-command=>\&undo)->pack(-side =>'left');
$modeFrame->Button(-text=>"Clear",-command=>\&clear)->pack(-side =>'left');
$buttonFrame=$f1->LabFrame(-label=>"Atoms")->pack(-side=>"left");
$buttonFrame->Button(-text=>"C",-command=>[\&setAtom,"C"])->pack(-side =>'left');
$buttonFrame->Button(-text=>"O",-command=>[\&setAtom,"O"])->pack(-side =>'left');
$buttonFrame->Button(-text=>"N",-command=>[\&setAtom,"N"])->pack(-side =>'left');
$modeLabel=$modeFrame->Label(-text=>"Current Mode : elements",-width=>24)->pack();
$moleculeControlFrame=$DrawingFrame->LabFrame(-label=>"Controls")->pack(-anchor=>"nw");
$moleculeControlFrame->Button(-text=>"Import",-width=>10,-command=>\&import)->pack(-anchor=>"nw");
$moleculeControlFrame->Button(-text=>"Export",-width=>10,-command=>\&export)->pack(-anchor=>"nw");
$f4=$moleculeControlFrame->Frame()->pack(-anchor=>"nw");
$f4->Button(-text=>"Template",-width=>10,-command=>\&setTemplate)->pack(-side=>"left",-anchor=>"nw");
$currTemplate=$f4->Entry(-width=>8)->pack(-anchor=>"nw",-side=>"left");
$moleculeCanvas=  $DrawingFrame->ChemCanvas(-width=>400,-height=>400);
$moleculeCanvas->pack(-fill=>"both",-expand=>1);


$templateFrame=$mw->LabFrame(-label=>"Template Canvas")->pack(-side=>"left",-anchor=>"nw");
$templateButtonFrame=$templateFrame->LabFrame(-label=>"Template Controls")->pack(-side=>"top",-anchor=>"nw");
$templateButtonFrame->Button(-text=>"New",-width=>8,-command=>\&clearTemplate)->pack(-side=>"top",-anchor=>"nw");
$f2=$templateButtonFrame->Frame()->pack(-anchor=>"nw");
$f2->Button(-text=>"Store",-width=>8,-command=>\&saveTemplate)->pack(-side=>"left");
$saveKeyEntry=$f2->Entry(-width=>8)->pack(-side=>"left");
$f3=$templateButtonFrame->Frame()->pack(-anchor=>"nw");
$f3->Button(-text=>"Show",-width=>8,-command=>\&showTemplate)->pack(-side=>"left");
$showKeyEntry=$f3->Entry(-width=>8)->pack(-side=>"left");
$templateCanvas=$templateFrame->ChemCanvas()->pack();

$currentAtom="C";
$currentCanvas=$moleculeCanvas;
sub switchTarget
  {
    if($currentCanvas == $moleculeCanvas)
      {
	$currentCanvas = $templateCanvas;
	$buttonTarget->configure(-text=>"Template");
      }	
    else
      {
	$currentCanvas=$moleculeCanvas;
	$buttonTarget->configure(-text=>"Molecule");
      }
  }	
sub setAtom
  {
    my $atom=shift;
    $currentCanvas->currentAtom($atom);
  }	
sub setMode
  {
    my $mode =shift;
    $currentCanvas->mode({-mode=>$mode});
    my $readMode=$currentCanvas->mode();
    $modeLabel->configure(-text=>"Current Mode: $readMode");
  }	
sub undo
  {
    $currentCanvas->undo();
  }	
sub clear
  {
    $currentCanvas->clear();
  }	


sub export
  {
    $molecule=$moleculeCanvas->molecule();
    $molecule->write('debug_out.sdf');
  }
sub import
  {
    $moleculeCanvas->molecule($molecule);
  }	
sub clearTemplate
  {
    $templateCanvas->clear();
  }	
sub saveTemplate
  {
    $key=$saveKeyEntry->get();
    $templates{$key}=$templateCanvas->molecule();
    store  \%templates, "templates.store"; 
  }	
sub showTemplate
  {
    $key=$showKeyEntry->get();
    $m=$templates{$key};
    $templateCanvas->molecule($m);
  }
sub setTemplate
  {
    $moleculeCanvas->template($templates{$currTemplate->get()});
  }	
MainLoop;

