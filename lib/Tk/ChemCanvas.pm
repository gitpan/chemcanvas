package Tk::ChemCanvas;
use version;our $VERSION=qv('1.0.0');
use Tk::widgets qw/Canvas/;
use base qw/Tk::Derived Tk::Canvas/;
use Carp;
use strict;

use Data::Dumper;
use only 'Chemistry::Mol' => q{0.4.0};
use Math::VectorReal;
use Math::Trig;
use Math::Round;

#use Smart::Comments;
 #Atom Colors courtesy Jmol http://jmol.sourceforge.net/jscolors/

my %AtomHexColors = (
   "H" => "#FFFFFF",
   "He" => "#D9FFFF",
   "Li" => "#CC80FF",
   "Be" => "#C2FF00",
   "B" => "#FFB5B5",
   "C" => "#909090",
   "N" => "#3050F8",
   "O" => "#FF0D0D",
   "F" => "#90E050",
   "Ne" => "#B3E3F5",
   "Na" => "#AB5CF2",
   "Mg" => "#8AFF00",
   "Al" => "#BFA6A6",
   "Si" => "#F0C8A0",
   "P" => "#FF8000",
   "S" => "#FFFF30",
   "Cl" => "#1FF01F",
   "Ar" => "#80D1E3",
   "K" => "#8F40D4",
   "Ca" => "#3DFF00",
   "Sc" => "#E6E6E6",
   "Ti" => "#BFC2C7",
   "V" => "#A6A6AB",
   "Cr" => "#8A99C7",
   "Mn" => "#9C7AC7",
   "Fe" => "#E06633",
   "Co" => "#F090A0",
   "Ni" => "#50D050",
   "Cu" => "#C88033",
   "Zn" => "#7D80B0",
   "Ga" => "#C28F8F",
   "Ge" => "#668F8F",
   "As" => "#BD80E3",
   "Se" => "#FFA100",
   "Br" => "#A62929",
   "Kr" => "#5CB8D1",
   "Rb" => "#702EB0",
   "Sr" => "#00FF00",
   "Y" => "#94FFFF",
   "Zr" => "#94E0E0",
   "Nb" => "#73C2C9",
   "Mo" => "#54B5B5",
   "Tc" => "#3B9E9E",
   "Ru" => "#248F8F",
   "Rh" => "#0A7D8C",
   "Pd" => "#006985",
   "Ag" => "#C0C0C0",
   "Cd" => "#FFD98F",
   "In" => "#A67573",
   "Sn" => "#668080",
   "Sb" => "#9E63B5",
   "Te" => "#D47A00",
   "I" => "#940094",
   "Xe" => "#429EB0",
   "Cs" => "#57178F",
   "Ba" => "#00C900",
   "La" => "#70D4FF",
   "Ce" => "#FFFFC7",
   "Pr" => "#D9FFC7",
   "Nd" => "#C7FFC7",
   "Pm" => "#A3FFC7",
   "Sm" => "#8FFFC7",
   "Eu" => "#61FFC7",
   "Gd" => "#45FFC7",
   "Tb" => "#30FFC7",
   "Dy" => "#1FFFC7",
   "Ho" => "#00FF9C",
   "Er" => "#00E675",
   "Tm" => "#00D452",
   "Yb" => "#00BF38",
   "Lu" => "#00AB24",
   "Hf" => "#4DC2FF",
   "Ta" => "#4DA6FF",
   "W" => "#2194D6",
   "Re" => "#267DAB",
   "Os" => "#266696",
   "Ir" => "#175487",
   "Pt" => "#D0D0E0",
   "Au" => "#FFD123",
   "Hg" => "#B8B8D0",
   "Tl" => "#A6544D",
   "Pb" => "#575961",
   "Bi" => "#9E4FB5",
   "Po" => "#AB5C00",
   "At" => "#754F45",
   "Rn" => "#428296",
   "Fr" => "#420066",
   "Ra" => "#007D00",
   "Ac" => "#70ABFA",
   "Th" => "#00BAFF",
   "Pa" => "#00A1FF",
   "U" => "#008FFF",
   "Np" => "#0080FF",
   "Pu" => "#006BFF",
   "Am" => "#545CF2",
   "Cm" => "#785CE3",
   "Bk" => "#8A4FE3",
   "Cf" => "#A136D4",
   "Es" => "#B31FD4",
   "Fm" => "#B31FBA",
   "Md" => "#B30DA6",
   "No" => "#BD0D87",
   "Lr" => "#C70066",
   "Rf" => "#CC0059",
   "Db" => "#D1004F",
   "Sg" => "#D90045",
   "Bh" => "#E00038",
   "Hs" => "#E6002E",
   "Mt" => "#EB0026",
);

Construct Tk::Widget 'ChemCanvas';


 sub Populate
  {
    my ($self,$args)=@_;
    $self->SUPER::Populate($args);
    $self->{ChemCanvasMode}="elements";
    $self->{ChemCanvasCurrentAtom}="C";
    $self->{ChemCanvasIndex}=0;
    $self->{ChemCanvasMol}=Chemistry::Mol->new(id => "ChemCanvasmol", );
    $self->{ChemCanvasUndoStack}=[];
    $self->{ChemCanvasBondLength}=50;
  }	

 sub ClassInit
  {
    my ($class , $mw)=@_;
    $class->SUPER::ClassInit($mw);
    $mw->bind($class,'<Button-1>' => \&button1);
    $mw->bind($class,'<Motion>'=>\&freeMotion);
#    $mw->bind($class,'<Button-3>'=> \&markAttachment);
  }	

sub freeMotion
  {
    my $self=shift;
    my $event =$self->XEvent;
    my $x=$self->canvasx($event->x);
    my $y=$self->canvasy($event->y);
    my @atoms=$self->{ChemCanvasMol}->atoms();
    return unless scalar @atoms;
    my $result=findNearestAtom({x=>$x,y=>$y},\@atoms);
    if($result->{distance} < 5)
      {
	my $coords=$result->{atom}->coords;
	my $ux=$coords->x - 5;
	my $uy=$coords->y - 5;
	my $bx=$coords->x + 5;
	my $by=$coords->y + 5;
	my $tag=$result->{atom}->id();
	$self->{ChemCanvasFreeSelect}={tag=>$tag,bb=>[$ux,$uy,$bx,$by]};
	$self->createOval($ux,$uy,$bx,$by,-tags=>$tag,-outline=>"blue",-fill=>"blue");
      }
    else
      {
	if($self->{ChemCanvasFreeSelect})
	  {
	    $self->delete($self->{ChemCanvasFreeSelect}->{tag});
	    $self->{ChemCanvasFreeSelect}=undef;
	  }	
	
      }	
  }	
##### 
# Public Class set/get method
# Arguments: hash ref with single key mode, possible values  elements,bonds,erase,chain,and template
# Returns: the mode
# Exceptions croaks in the value is not valid
sub mode
  {
    my ($self,$args)=@_;
    if($args)
      {
	my $mode=$args->{-mode}? $args->{-mode}:"elements";
	$mode=lc($mode);
	croak("unrecognized value for -mode : $mode") if $mode ne "elements" and 
                                                     $mode ne "bonds"    and 
                                                     $mode ne "erase"    and 
						     $mode ne "chain"    and 
						     $mode ne "template";
	$self->{ChemCanvasMode}=$mode;
	return $mode;
      }
    else
      {
	return $self->{ChemCanvasMode};
      }

  }
##### 
#  Private Class method
# Arguments: the XEvent
# Returns: nothing
# Exceptions: none
# Additional Info this method is used to mark teh attachment atom in a template
sub markAttachment
  {
    my $self=shift;
    my $event=$self->XEvent;
    
    my @atoms=$self->{ChemCanvasMol}->atoms();
    my $result=findNearestAtom({x=>$self->canvasx($event->x),y=>$self->canvasy($event->y)},\@atoms);
    if($result->{distance} < 5)
      {
	$result->{atom}->attr(attachment=>"atom");
	$self->drawAtom($result->{atom}->id,
			$result->{atom}->coords,
			$result->{atom}
		       );
	return;
      }	
    my $tag=$self->currentTag();
    if($tag)
      {
	my $item=$self->{ChemCanvasMol}->by_id($tag);
	if($item->isa("Chemistry::Bond"))
	  {
	    $self->drawBond($item);
	    $item->attr(attachment=>"bond");
	  }
      }
  }	
##### 
#  private class method
# Arguments: none
# Returns: the current tag's unique ID 
#          returns undef if there is not current tag
# Exceptions none
# Additional Info each item on the canvas has a tag that is its unique ID.
sub currentTag
  {
    my $self=shift;
    my @taglist=$self->gettags("current");
    if(@taglist)
      {
	my $tag;
	foreach my $tagiter (@taglist)
	  {
	    if($tagiter ne "current")
	      {
		$tag=$tagiter;
		last;
	      }
	  }
	return $tag;
      }
    return undef;
  }
##### 
# Private Class Entry Point method
# Arguments: none
# Returns: nothing
# Exceptions none
# Additional Info This is the main entry point. it is bound to a <Button-1> event
#                 Based on the mode it calls various funciton to impliment the functionality 
#                 of the ChemCanvas
# Bindings <Button-1> all the time
sub button1
  {
    my $self=shift;
    my $event= $self->XEvent;
    my @atoms=$self->{ChemCanvasMol}->atoms();
    my $result=findNearestAtom({x=>$self->canvasx($event->x),y=>$self->canvasy($event->y)},\@atoms);
    my $tag=$self->currentTag();
    if( $result && !$tag)
      {
	if($result->{distance} > 10)#$self->{ChemCanvasBondLength} *2)
	  {
	    $self->scanMark($event->x,$event->y);
	    $self->CanvasBind("<Motion>",\&scanning);
	    $self->CanvasBind("<ButtonRelease-1>",\&endScan);
	    return;
	  }
      }
    if ($self->{ChemCanvasMode} eq "elements")
      {
	if(! $self->{ChemCanvasMol}->atoms())
	  {
	    $self->addAtom({x=>$self->canvasx($event->x),y=>$self->canvasy($event->y)});
	    return;
	  }
	my $result=findNearestAtom({x=>$self->canvasx($event->x),y=>$self->canvasy($event->y)},\@atoms);
	if ($result->{distance} < 5)
	  {
	    $self->changeAtom($result->{atom}->id());
	    return;
	  }	
      }
    elsif($self->{ChemCanvasMode} eq "bonds" )
      {
	my @taglist=$self->gettags("current");
	if(@taglist)
	  {
	    my $tag=$self->currentTag();
	    if($tag=~m/txt/)
	      {return}
	    my $item=$self->{ChemCanvasMol}->by_id($tag);
	    if($item->isa("Chemistry::Bond"))
	      {
		$self->changeBondOrder($tag);
	      }
	    else
	      {
		push @{$self->{ChemCanvasUndoStack}},{op=>"endContainer"};
		$self->CanvasBind('<ButtonRelease-1>'=>\&buttonRelease1);
		$self->startBond($event);
	      }	
	    return;
	  }
	if( ! $self->{ChemCanvasMol}->atoms())
	  {
	    push @{$self->{ChemCanvasUndoStack}},{op=>"endContainer"};
	    $self->CanvasBind('<ButtonRelease-1>'=>\&buttonRelease1);
	    $self->startBond($event);
	  }	
      }	
    elsif ($self->{ChemCanvasMode} eq "chain" )
      {
	my $tag=$self->currentTag();
	if($tag)
	  {
	    $self->startChain($event);
	    return;
	  }
	if(! $self->{ChemCanvasMol}->atoms())
	  {
	    $self->startChain($event);
	  }	
      }	
    elsif($self->{ChemCanvasMode} eq "erase")
      {
	my $tag=$self->currentTag();
	if($tag)
	  {
	    my $item=$self->{ChemCanvasMol}->by_id($tag);
	    if($item->isa("Chemistry::Atom"))
	      {
		$self->deleteAtom($tag);
	      }	
	    elsif($item->isa("Chemistry::Bond"))
	      {
		$self->deleteBond($tag);
	      }	
	  }
      }
    elsif($self->{ChemCanvasMode} eq "template")
      {
	$self->placeTemplate($event);
      }	
  }
##### 
#  Private Class Entry Point method
# Arguments: none
# Returns: nothing
# Exceptions none
# Additional Info This is the entrypoint for the drag behavior of the ChemCanvas
# bindings <Motion> when a canvas drag is happening

sub scanning
  {
    my $self=shift;
    my $event=$self->XEvent;
    $self->scanDragto($event->x,$event->y,1);
  }	
##### 
# Private Class  method
# Arguments: none
# Returns: none
# Exceptions none
# Additional Info Called at the end of a drag to clear the drag bindings
# Bindings <ButtonRelease-1> durring a drag
sub endScan
  {
    my $self=shift;
    $self->CanvasBind("<Motion>",\&freeMotion);
    $self->CanvasBind("<ButtonRelease-1>","");
  }
##### 
#  Private Class method
# Arguments: a Chemistry::Mol object
# Returns: the modified Chemistry::Mol object
# Exceptions none
# Additional Info This method deletes the overlap and attachement attributes 
#                 the atoms and bonds of the argument Mol
sub clearAttachmentInfo
  {
    my $self=shift;
    my $template=shift;
    
    foreach($template->atoms())
      {
	$_->del_attr("overlap");
	$_->del_attr("attachment")
      }
    foreach($template->bonds())
      {
	$_->del_attr("overlap");
	$_->del_attr("attachment")
      }	
    return $template;
  }
##### 
#  Private Class Entry Point method
# Arguments: XEvent
# Returns: nothing
# Exceptions none
# Additional Info This function places the current template at the selected structure (bond or atom)
sub placeTemplate
  {
    my $self=shift;
    my $event=shift;
    my $newTemplate=$self->{ChemCanvasTemplate};
    my @atoms=$self->{ChemCanvasMol}->atoms();
    push @{$self->{ChemCanvasUndoStack}},{op=>"endContainer"};
    if( ! scalar @atoms)#no atoms 
      {
	$newTemplate=$newTemplate->safe_clone();
	$newTemplate=$self->translateAttachment($newTemplate,Math::VectorReal->new($self->canvasx($event->x),$self->canvasy($event->y),0));
        $self->clearAttachmentInfo($newTemplate);
	push @{$self->{ChemCanvasUndoStack}},{op=>"placeTemplate", m=>Chemistry::Mol->new()};
	$self->molecule($newTemplate);
	push @{$self->{ChemCanvasUndoStack}},{op=>"startContainer"};
	return;
      }

    #determine if the uses clicked an atom or a bond
    my $currentTag=$self->currentTag();
    if($currentTag)
      {
	my $item=$self->{ChemCanvasMol}->by_id($currentTag);
	if($item->isa("Chemistry::Bond"))
	  {
	    $self->attachAtBond($newTemplate->safe_clone(),$event,$item);
	  }
	elsif($item->isa("Chemistry::Atom"))
	  {
	    $self->attachAtAtom($newTemplate->safe_clone(),$event,$item);
	  }
      }
    push @{$self->{ChemCanvasUndoStack}},{op=>"startContainer"};
  }
##### 
#  Private Class method
# Arguments: a Chemistry::Mol object that is the current template, the XEvent, and the bond selected
# Returns: nothing
# Exceptions :none
# Additional Info places the template argument at the bond argument
sub attachAtBond
  {
    my $self=shift;
    my $newTemplate=shift;
    my $event=shift;
    my $clickedBond=shift;
    #verify that the template has a attachment bond
    my @bonds=$newTemplate->bonds();
    my $attachBond;
    foreach(@bonds)
      {
	if($_->attr("attachment"))
	  {
	    $attachBond=$_;
	  }	
      }	
    return unless $attachBond;
    push @{$self->{ChemCanvasUndoStack}},{op=>"placeTemplate", m=>$self->{ChemCanvasMol}->clone};
    #get formal angle of clicked bond
    my $clickedAngle=$self->formalBondAngle($clickedBond->atoms());
    #get formal angle of attach bond
    my $attachAngle=$self->formalBondAngle($attachBond->atoms());
    #now translate one end of the attachement bond ot 0,0
    #in preperation to rotate it;
    my @atoms=$attachBond->atoms();
    my @canidates;
    for my $i(1,0)
      {
	my $angle;
	if($i){$angle=180;}else{$angle=0;}
	my $canidateTemplate=$newTemplate->clone;
	my $v=$atoms[$i]->coords();
	my $diff=$v-Math::VectorReal->new(0,0,0);
	$canidateTemplate=$self->translate($canidateTemplate,$diff);
	$canidateTemplate=$self->rotate($clickedAngle-$attachAngle+$angle,$canidateTemplate);
	my @atomsClicked=$clickedBond->atoms();
	$v=$atomsClicked[0]->coords();
	$diff=Math::VectorReal->new(0,0,0)-$v;
	$canidateTemplate=$self->translate($canidateTemplate,$diff);
	my $overlapCount=$self->markOverlappingAtoms($canidateTemplate);
	push @canidates,{count=>$overlapCount,canidate=>$canidateTemplate};
      }
    if($canidates[0]->{count} > $canidates[1]->{count})
      {
	$newTemplate=$canidates[1]->{canidate};
      }	
    else
      {
	$newTemplate=$canidates[0]->{canidate};
      }	
    #conect in the template
    $self->mergeTemplate($newTemplate);
    $self->delete("all");
    $self->drawMolecule($self->{ChemCanvasMol});
  }
##### 
# Private Class  method
# Arguments: A Chemistry::Mol object
# Returns: nothing
# Exceptions none
# Additional Info merges the now placed template with the molecule
sub mergeTemplate
  {
    my $self=shift;
    my $newTemplate=shift;
    my @deleteAtoms;
    my $bondOrder;
    $self->{ChemCanvasMol}->combine($newTemplate);
    foreach($newTemplate->atoms())
      { 
	my $overlapAtomId;
	my $targetAtom;
	if($overlapAtomId=$_->attr("overlap"))
	   {
	     #conect the overlaped atom to the same atoms the template atom is
	     #then delete the template atom
	     my $overlapAtom=$self->{ChemCanvasMol}->by_id($overlapAtomId);
	     my @neighbors=$_->neighbors($_);
	     #need to create a bond between the neighors and the overlapatom
	     foreach my $n(@neighbors)
	       {
		 my $bond=$self->findBondByAtoms($_,$n);
		 $bondOrder=$bond->order();
		 if( $n->attr("overlap"))
		   {
		     
		     my $overlapAtom2Id=$n->attr("overlap");
		     my $overlapAtom2=$self->{ChemCanvasMol}->by_id($overlapAtom2Id);
		     #basicly we want to check if the 2 overlap atoms are bonded in the main molecule
		     #if not we need to bond them
		     my @overlapNeighbors=$overlapAtom->neighbors();
		     my $bonded=0;
		     foreach my $on(@overlapNeighbors)
		       {
			 if($on == $overlapAtom2)
			   {
			     $bonded=1;
			   }
		       }
		     if(!$bonded)
		       {
			 $self->{ChemCanvasMol}->new_bond(atoms=>[$overlapAtom,$overlapAtom2],order=>$bondOrder);
		       }	
		     next;
		   }	
		 my $nid=$n->id();
		 my $newN=$self->{ChemCanvasMol}->by_id($nid);
		 my $tempBond=$self->{ChemCanvasMol}->new_bond(atoms=>[$newN,$overlapAtom],order=>$bondOrder);
	       }	
	   }	
	 }
    my @atoms=$self->{ChemCanvasMol}->atoms();
    foreach(@atoms)
      {
	if($_->attr("overlap"))
	  {
	    $self->{ChemCanvasMol}->delete_atom($_);
	  }	
      }
    $self->clearAttachmentInfo($self->{ChemCanvasMol});
  }	
##### 
#  Private helper method
# Arguments: 2 Chemistry::Atom obj
# Returns: the Chemistry::Bond obj that binds the arguments
# Exceptions croaks if there is no such bond
# Additional Info
sub findBondByAtoms
  {
    my $self=shift;
    my $atom1=shift;
    my $atom2=shift;
    my @bonds=$atom1->bonds();
    my $matchedBond;
    foreach my $b (@bonds)
      {
	my @bondAtoms=$b->atoms();
	#each bond should have 2 atoms
	my $match1=0;
	my $match2=0;
	foreach my $a(@bondAtoms)
	  {
	    if ($a == $atom1)
	      {
		$match1=1;
	      }	
	    if ($a == $atom2)
	      {
		$match2=1;
	      }	
	  }
	if($match1 && $match2)
	  {
	    $matchedBond=$b;
	    last;
	  }
      }	
    
    return  $matchedBond;
  }	
##### 
# Private Class  method
# Arguments: the template molecule ,the XEvent, and the clicked atom
# Returns: nothing
# Exceptions nothing
# Additional Info Called when placing a template at an atom. determines the correct rotation and translation of the 
#                 template. then merges the template and the molecule.
#                 Algorithm: if the template attach atom has more than one bond.
#                 find all of the bonds at the attachment atom. if only 1 , place the the template at 120 deg
#                 if there are more than 1 bond, find the largest angle and bisect it.
#                 if the atachemt atom of the template has more that one bond, then attach the template with
#                 a single bond at an angle calculated as before.
sub attachAtAtom
  {
    my $self=shift;
    my $newTemplate=shift;
    my $event=shift;
    my $clickedAtom=shift;
    #verify that the template has an attachemnt atom
    my @atoms=$newTemplate->atoms();
    my $attachAtom;
    foreach (@atoms)
      {
	if($_->attr("attachment"))
	  {
	    $attachAtom=$_;
	  }	
      }
    return unless $attachAtom;
    push @{$self->{ChemCanvasUndoStack}},{op=>"placeTemplate", m=>$self->{ChemCanvasMol}->clone};
    my @clickedBonds=$clickedAtom->bonds();
    my $bondCount=scalar @clickedBonds;
    my $targetAtom;
    if( $bondCount == 0 or $bondCount == 1)#share the atom
      {
	$targetAtom=$clickedAtom;
      }	
    else# create a single bond to attach at the attach atom
      {
	$self->startBond($event);
	my $createdBond=$self->endBond($event);
	my @atoms=$createdBond->atoms();
	foreach (@atoms)
	  {
	    if ($_ != $clickedAtom)
	      {
		$targetAtom=$_;
		last;
	      }	
	  }	
      }
    $self->itemconfigure($targetAtom->id(),-fill=>"red");
    #now the target atom can be shared in the same manner
    my $angle=$self->calcTemplateAngle($newTemplate,$targetAtom,$attachAtom);
    $self->rotate($angle,$newTemplate);
    #the atachment atom is already at 0,0
    my $v=$targetAtom->coords();
    $v=$v*-1;
    $self->translate($newTemplate,$v);
    my $v=$targetAtom->coords();
    $v=$v*-1;
    $self->markOverlappingAtoms($newTemplate);
    $self->mergeTemplate($newTemplate);
    $self->delete("all");
    $self->drawMolecule($self->{ChemCanvasMol});
  }
##### 
#  private Class method
# Arguments: the template, the target atom(the atom in teh molecule where the user clicked, and the template attachement atom
# Returns: the angle from north.
# Exceptions none
# Additional Info
sub calcTemplateAngle
  {
    my $self=shift;
    my $template=shift;
    my $targetAtom=shift;
    my $attachAtom=shift;
    my $angle=0;
    my @targetBonds=$targetAtom->bonds();
    my $targetBond=$targetBonds[0];# the target atom is constructed so that is always has one bond
    my $targetBondAngle=$self->formalBondAngle($targetAtom,$targetAtom->neighbors);
    my @attachBonds=$attachAtom->bonds();
    my @n=$attachAtom->neighbors();
    my @angles;
    foreach (@n)
      {
	my $fa=$self->formalBondAngle($attachAtom,$_);
	push @angles,{angle=>$fa,atom=>$_};
	
      }
    if(scalar @angles== 1)#the attach atom has only 1 bond
      {
	my $largest=120+$angles[0]->{angle};#preference for 120 degree angle similar to benzene
	$angle=$targetBondAngle-$largest;
      }	
    else
      {
     
	#sort the angles into the formal angle order
	@angles=sort {$a->{angle}<=>$b->{angle}} @angles;
	#measure the angle between each 
	my @measuredAngles;
	my $sum=0;
	for (0 ..  (scalar @angles -2))
	  {
	    my $ma=$self->measureAngle($attachAtom->coords-$angles[$_]->{atom}->coords,
				       $attachAtom->coords-$angles[$_+1]->{atom}->coords
				      );
	    push @measuredAngles,{measured=>$ma,startA=>$angles[$_],endA=>$angles[$_+1]};
	    $sum=$sum+$ma;
	  }
	
	my $ma=360-$sum;
	push @measuredAngles,{measured=>$ma,startA=>$angles[-1],endA=>$angles[0]};
	#sort in decending order
	@measuredAngles=sort {$b->{measured}<=>$a->{measured}}@measuredAngles;
	my $largest=$measuredAngles[0];
	my $formalLargest=$largest->{measured}/2 +$largest->{startA}->{angle};
	#rotate the formalLargest to the targetbondangle
	$angle=$targetBondAngle-$formalLargest;
      }
    return $angle;
  }
##### 
# private class method
# Arguments: the template. it should be rotated and translated already
# Returns: returns the number of atoms that overlap between the template and the molecule
# Exceptions none
# Additional Info an atom overlaps if the distance between teh centers of the atoms is less that 3	
sub markOverlappingAtoms
  {
    my $self=shift;
    my $template=shift;
    my $overlapp=0;
    foreach my $ta($template->atoms())
      {
	foreach my $ma($self->{ChemCanvasMol}->atoms())
	  {
	    my $d=distance($ta->coords(),$ma->coords());
	    if($d <3)
	      {
		$ta->attr("overlap",$ma->id());
		$overlapp++;
		last;
	      }	
	  }
      }
    return $overlapp;
  }
##### 
#  private classmethod
# Arguments:  the XEvent
# Returns: nothing
# Exceptions none
# Additional Info binds the <motion> event
# bindings called when in the chain mode and the user clicks	
sub startChain
  {
    my $self=shift;
    my $event=shift;
    push @{$self->{ChemCanvasUndoStack}},{op=>"endContainer"};
    $self->CanvasBind("<ButtonRelease-1>",\&endChain);
    $self->CanvasBind("<Motion>",\&chain);
    $self->bondSetup($event);
    $self->setUpBondAngles(60);
    #calculate the 60 deg angles
    $self->{ChemCanvasCurrentBondEndpoint}=undef;
    $self->{ChemCanvasCurrentBondTag}=undef;
  }	
##### 
#  private method
# Arguments: the XEvent
# Returns: nothing
# Exceptions none
# Additional Info draws a chain of atoms at alternating 120 degree angles. but does not 
#                 actually create the atoms
sub chain
  {
    my $self=shift;
    my $event=$self->XEvent;
    my $x=$self->canvasx($event->x);
    my $y=$self->canvasy($event->y);
    $self->delete("chain");
    my @endpoints=@{$self->{ChemCanvasBondEndpoints}};
    my $minEndpoint;
    my $minDistance=1000000000;
    my $distance;
    foreach my $endpoint(@endpoints)
      {
	$distance=distance($endpoint,{x=>$x,y=>$y});
	if($distance<$minDistance)
	  {
	    $minDistance=$distance;
	    $minEndpoint=$endpoint;
	  }
      }

    my $distanceFromOrigin=sqrt(($self->{ChemCanvasBondOrigin}->{x}-$self->canvasx($event->x))**2+($self->{ChemCanvasBondOrigin}->{y}-$self->canvasy($event->y))**2);

    my $v=Math::VectorReal->new(($minEndpoint->{x}-$self->{ChemCanvasBondOrigin}->{x}),($minEndpoint->{y}-$self->{ChemCanvasBondOrigin}->{y}),0);
    $v=$v->norm;
    my $bondVector=$v*$self->{ChemCanvasBondLength};
    my $vLine=$v*$distanceFromOrigin;
    #A point <x,y> can be rotated around the origin <0,0> by running it through the following equations to get the new point <x',y'> :
    #x' = cos(theta)*x - sin(theta)*y 
    #y' = sin(theta)*x + cos(theta)*y
    #where theta is the angle by which to rotate the point.
    my $numBonds=int($distanceFromOrigin/($self->{ChemCanvasBondLength}*cos(deg2rad(30))));
    my $pseudoOrigin=$self->{ChemCanvasBondOrigin};
    my $inverter=-1;
    my $angle=30;
    my @atomLocations;
    for my $i (0 ..$numBonds-1)
      {
	push @atomLocations,$pseudoOrigin;
	my $xprime=$bondVector->x*cos(deg2rad($angle)) - $bondVector->y*sin(deg2rad($angle));
	my $yprime=$bondVector->x*sin(deg2rad($angle))+ $bondVector->y*cos(deg2rad($angle));
	$self->createLine($pseudoOrigin->{x},$pseudoOrigin->{y},$xprime+$pseudoOrigin->{x},$yprime+$pseudoOrigin->{y},-tags=>"chain");
	$pseudoOrigin={x=>$xprime+$pseudoOrigin->{x},y=>$yprime+$pseudoOrigin->{y}};
	$angle=$angle*$inverter;
      }	
    push @atomLocations,$pseudoOrigin;    
    $self->{ChemCanvasChainAtomLocations}=\@atomLocations;
  }
##### 
#  private class method
# Arguments: none
# Returns: none
# Exceptions none
# Additional Info called when the user releases the drag. creates the atoms	
sub endChain
  {
    my $self=shift;
    $self->delete("chain");
    $self->CanvasBind("<Motion>","");
    $self->CanvasBind("<ButtonRelease-1>","");
    my @desiredAtomLocations=@{$self->{ChemCanvasChainAtomLocations}};
    my @atomLocations=$self->allAtomLocations();
    my @creationLocations;
    foreach my $a (@desiredAtomLocations)
      {
	#verify there is no atom near this location
	my $close=0;
	foreach my $l(@atomLocations)
	  {
	    if (isClose($a,$l->{l}))
	      {
		$close =$l;
	      }	
	  }	
	
	if(!$close)
	  {
	    push @creationLocations,{l=>$a,i=>""};
	  }	
	else
	  {
	    $close->{close}=1;
	    push @creationLocations,$close;
	  }
      }
    #create the atoms
    foreach my $a(@creationLocations)
      {
	if( ! $a->{i})
	  {
	    $a->{i}=$self->addAtom($a->{l});
	  }
      }
    #create the bonds
    my $end=scalar @creationLocations-2;
    for my $i(0..$end)
      {
	unless ($creationLocations[$i+1]->{close} && $creationLocations[$i]->{close})
	  {
	    $self->createBond($creationLocations[$i]->{i},$creationLocations[$i+1]->{i},1);
	  }
      }
    push @{$self->{ChemCanvasUndoStack}},{op=>"startContainer"};    
  }
##### 
#  private helper method
# Arguments: 2 point hashes
# Returns: boolean
# Exceptions none
# Additional Info true if the point are closer that 25
sub isClose
  {
    my $p1=shift;
    my $p2=shift;
    my $d=($p2->{x}-$p1->{x})**2+($p1->{y}-$p2->{y})**2;
    if ($d >25 )
      {
	return 0;
      }	
    else
      {return 1;}
  }	
##### 
# private class method
# Arguments: none
# Returns: an array of hashes that contains 2 keys, 
#             l=> a point hash
#             i-> the id of the atom at l
# Exceptions
# Additional Info
sub allAtomLocations
  {
    my $self=shift;
    my @atoms=$self->{ChemCanvasMol}->atoms();
    my @atomLocations;
    foreach my $a (@atoms)
      {
	my $v=$a->coords();
	push @atomLocations,{l=>{x=>$v->x,y=>$v->y},i=>$a->id()};
      }
    return @atomLocations;
  }	
##### 
# method
# Arguments: xevent
# Returns: none
# Exceptions none
# Additional Info this is called to end a bond creation
sub buttonRelease1
  {
    my $self=shift;
    my $event= $self->XEvent;
    $self->CanvasBind("<ButtonRelease-1>"=>"");
    $self->CanvasBind("<Motion>","");
    if($self->{ChemCanvasMode} eq "bonds")
      {
	$self->endBond($event,1);
	
      }	
    push @{$self->{ChemCanvasUndoStack}},{op=>"startContainer"};
  }	
##### 
# Public class accessor method
# Arguments: optional chemical symbol
# Returns: the current chemical symbol
# Exceptions croaks if the chemical symbol is not valid
# Additional Info
sub currentAtom
  {
    my $self=shift;
    if(@_)
      {
	my $atom=shift;
	croak("$atom is not a valid chemical symbol") unless exists $AtomHexColors{$atom};
	$self->{ChemCanvasCurrentAtom}=$atom;
	return $self->{ChemCanvasCurrentAtom};
      }
    else
      {
	return $self->{ChemCanvasCurrentAtom};
      }	
  }
##### 
#  private class method
# Arguments:  a point hash, an optional tag that is used for undo
# Returns: the id of the atom created
# Exceptions none
# Additional Info creates an atom at the p
sub addAtom
  {
    my $self=shift;
    my $p=shift;
    my $newTag=shift;
    my $atom;
    if($newTag)
      {
	$atom=$self->{ChemCanvasMol}->new_atom(symbol=>$self->{ChemCanvasCurrentAtom},coords=>[$p->{x},$p->{y},0],id=>$newTag);
      }
    else
      {
	$atom=$self->{ChemCanvasMol}->new_atom(symbol=>$self->{ChemCanvasCurrentAtom},coords=>[$p->{x},$p->{y},0]);
      }
    $self->drawAtom($atom->id,$p,$atom);
    push @{$self->{ChemCanvasUndoStack}},{op=>"deleteAtom",id=>$atom->id()} unless $newTag;
    return $atom->id();
  }
##### 
# private class method
# Arguments: a atom id, and a the symbol to change to
# Returns: nothing
# Exceptions none
# Additional Info changes the atom clicked to the new symbol	
sub changeAtom
  {
    my $self=shift;
    my $tag=shift;
    my $targetSymbol=shift;
    my $atom=$self->{ChemCanvasMol}->by_id($tag);
    my $originalSymbol=$atom->symbol();
    return unless $atom->isa("Chemistry::Atom");
    if( $targetSymbol)
      {
	$atom->symbol($targetSymbol);
      }	
    else
      {
	$atom->symbol($self->{ChemCanvasCurrentAtom});
      }	
    my $c=$atom->coords();
    $self->drawAtom($tag,{x=>$c->x,y=>$c->y},$atom);
    push @{$self->{ChemCanvasUndoStack}},{op=>"changeAtom",id=>$tag, element=>$originalSymbol} unless $targetSymbol;
  }
##### 
# private class method
# Arguments: an atom id, a point hash, and the atom
# Returns: none
# Exceptions none
# Additional Info draws the atom on the canvas
my $valenceTable={O=>2 , S=>2 , N=>3 , C=>4 , P=>3};
sub drawAtom
  {
    my $self=shift;
    my $tag=shift;
    my $p=shift;
    my $atom=shift;

    if(ref $p eq "Math::VectorReal")
      {
	my $hash={x=>$p->x,y=>$p->y};
	$p=$hash;
      }
    my $x=$p->{x};
    my $y=$p->{y};
  
    my $ux=$x-5;
    my $uy=$y-5;
    my $bx=$x+5;
    my $by=$y+5;
    $self->delete($tag);
    $self->delete($tag."txt");
    $self->delete($tag."txtbb");
    my $normalValence=$valenceTable->{$atom->symbol()};
    my $implicitH=$atom->calc_implicit_hydrogens();
    my $hydrogenTag;
    if($implicitH == 0)
      {
	$hydrogenTag="";
      }	
    elsif($implicitH == 1)
      {
	$hydrogenTag="H";
      }	
    else
      {
	$hydrogenTag="H".$implicitH;
      }
    my $placeTag=0;
    if ($atom->symbol() ne "C")
      {
	$placeTag=1;
      }
    if($normalValence)
      {
	#calculate the positive charge values
	if($implicitH==4 && $atom->symbol() eq "C")
	  {
	    $placeTag=1;
	  }
	my $valence=$atom->valence();
	if($valence > $normalValence)
	  {	
	    $valence=$valence-($normalValence+1);
	    $hydrogenTag="+";
	    if($valence)
	      {
		$hydrogenTag=($valence+1).$hydrogenTag;
	      }	
	    $placeTag=1;
	  }	
      }	

    if($placeTag)
      {
	$self->createText($x,$y,-tags=>$tag."txt",-text=>$atom->symbol().$hydrogenTag,-fill=>$AtomHexColors{$atom->symbol});
        my @bbox=$self->bbox($tag."txt");
	my $bgc=$self->cget('-background');
	$self->createRectangle(@bbox,-fill=>$bgc,-tags=>$tag."txtbb",-outline=>$bgc);
	$self->lower($tag."txtbb",$tag."txt");
      }
    if($atom->attr("attachment"))
      {
	$self->createText($ux+11,$uy-10,-tags=>$tag."txt",-text=>"attach");	
      }
    if($self->{ChemCanvasFreeSelect})
      {		
	$self->createOval(@{$self->{ChemCanvasFreeSelect}->{bb}},-tags=>$self->{ChemCanvasFreeSelect}->{tag},-outline=>"blue",-fill=>"blue");
      }	
  }
  	
##### 
# private class method
# Arguments:  an atom id, isUndo
# Returns: nothing
# Exceptions none
# Additional Info deletes the atom, and any bonds associated with that atom
# undo supported
sub deleteAtom
  {
    my $self=shift;
    my $tag=shift;
    my $isUndo=shift;
    my $atom=$self->{ChemCanvasMol}->by_id($tag);
    return unless $atom;
    my $coords=$atom->coords();
    my $p={x=>$coords->x,y=>$coords->y};
    my @bonds=$atom->bonds();
    push @{$self->{ChemCanvasUndoStack}},{op=>"endContainer"} unless $isUndo;
    foreach my $bond(@bonds)
      {
	$self->deleteBondOnly($bond->id(),$isUndo);
      }	
    push @{$self->{ChemCanvasUndoStack}},{op=>"addAtom",id=>$tag,p=>$p}unless $isUndo;
    $self->{ChemCanvasMol}->delete_atom($atom);
    push @{$self->{ChemCanvasUndoStack}},{op=>"startContainer"}unless $isUndo;
    $self->delete($tag);
    $self->delete($tag."txt");
    $self->delete("all");
    $self->drawMolecule($self->{ChemCanvasMol});
  }
##### 
# class helper method
# Arguments: an atom id
# Returns: nothing
# Exceptions none
# Additional Info deletes only the atom,not recursive, does not delete bonds.
#                 should only be called on atoms without any bonds or there will
#                 be undo problems
sub deleteAtomOnly
  {
    my $self=shift;
    my $tag=shift;
    my $isUndo=shift;
    my $atom=$self->{ChemCanvasMol}->by_id($tag);
    return unless $atom;
    my $coords=$atom->coords();
    my $p={x=>$coords->x,y=>$coords->y};
    push @{$self->{ChemCanvasUndoStack}},{op=>"addAtom",id=>$tag,p=>$p}unless $isUndo;
    $self->{ChemCanvasMol}->delete_atom($atom);
    $self->delete($tag);
    $self->delete($tag."txt");
  }	
##### 
# private class method
# Arguments: a bond id
# Returns: nothing
# Exceptions none
# Additional Info deletes a bond and any atom that was part of the bond, but had no other bonds
sub deleteBond
  {
    my $self=shift;
    my $tag=shift;
    my $isUndo=shift;
    $self->delete($tag);
    my $bond=$self->{ChemCanvasMol}->by_id($tag);
    return unless $bond;
    my @atoms=$bond->atoms();
    my $order=$bond->order();
    my $rec;
    my @ids;
    foreach my $atom(@atoms)
      {
	push @ids,$atom->id();
	my $coords=$atom->coords();
      }
    push  @{$self->{ChemCanvasUndoStack}},{op=>"endContainer"} unless $isUndo;
    $rec={op=>"createBond",id=>$tag,atoms=>[@ids],order=>$order};
    push @{$self->{ChemCanvasUndoStack}},$rec unless $isUndo;
    $self->{ChemCanvasMol}->delete_bond($bond);

     foreach my $atom(@atoms)
      {
	my @bonds=$atom->bonds();
	if(@bonds)
	  {
	    $self->drawAtom($atom->id(),$atom->coords(),$atom);
	  }
	else
	  {
	    $self->deleteAtomOnly($atom->id());
	  }
      }
    push  @{$self->{ChemCanvasUndoStack}},{op=>"startContainer"} unless $isUndo;
  }
##### 
#  class helper method
# Arguments: a bond id
# Returns: nothing
# Exceptions none
# Additional Info deletes only the bond but not any atoms that were part of that bond
sub deleteBondOnly
  {
    my $self=shift;
    my $tag=shift;
    my $isUndo=shift;
    $self->delete($tag);
    my $bond=$self->{ChemCanvasMol}->by_id($tag);
    return unless $bond;
    my @atoms=$bond->atoms();
    my $order=$bond->order();
    my $rec;
    my @ids;
    foreach my $atom(@atoms)
      {
	push @ids,$atom->id();
	my $coords=$atom->coords();
      }
    $rec={op=>"createBond",id=>$tag,atoms=>[@ids],order=>$order};
    push @{$self->{ChemCanvasUndoStack}},$rec unless $isUndo;
    $self->{ChemCanvasMol}->delete_bond($bond);
  }	
##### 
# class helper method
# Arguments: none
# Returns: a unique for this program id
# Exceptions none
# Additional Info
sub getIndex
  {
    my $self=shift;
    my $index=$self->{ChemCanvasIndex};
    $self->{ChemCanvasIndex}=$index+1;
    return $index;
  }	
##### 
# public class method
# Arguments: none
# Returns: nothing
# Exceptions none
# Additional Info this call is the entry point for undo. 
#            the undo stack contains the operations to undo. a operation is either
#            a single entry in the stack   or
#            several entries enclosed in  a start or end container.
sub undo
  {
    my $self=shift;
    my $op=pop @{$self->{ChemCanvasUndoStack}};
    return unless $op;
    if($op->{op} eq "addAtom")
      {
	$self->addAtom($op->{p},$op->{id});
      }
    elsif($op->{op} eq "deleteAtom")
      {
	$self->deleteAtom($op->{id},1);
      }	
    elsif($op->{op} eq "deleteBond")
      {
	$DB::single=1;
	$self->deleteBond($op->{id},1);
      }	
    elsif($op->{op} eq "createBond")
      {
	$self->createBond($op->{atoms}->[0],$op->{atoms}->[1],$op->{order},1);
      }	
    elsif($op->{op} eq "changeBondOrder")
      {
	$self->changeBondOrder($op->{id},$op->{order});
      }	
    elsif($op->{op} eq "changeAtom")
      {
	$self->changeAtom($op->{id},$op->{element});
      }	
    elsif($op->{op}eq "startContainer")
      {
	my $peek=$self->{ChemCanvasUndoStack}->[-1];
	if(!defined $peek)
	  { return ;}
	while($peek->{op} ne "endContainer")
	  {
	    $self->undo();
	    $peek=$self->{ChemCanvasUndoStack}->[-1];
	    if(!defined $peek)
	      { return ;}
	  }
	pop @{$self->{ChemCanvasUndoStack}};#clear the endContainer
      }
    elsif($op->{op} eq "endContainer")
      {
	return;
      }
    elsif($op->{op} eq "placeTemplate")
      {
	my $m=$op->{m};
	$self->{ChemCanvasMol}=$m;
	$self->delete("all");
	if($m)
	  {
	    $self->drawMolecule($self->{ChemCanvasMol});
	  }	
      }	
  }	
##### 
# class helper method
# Arguments:  the xevent
# Returns: nothing
# Exceptions none
# Additional Info this fucntion takes a piont hash, finds the close atom, if it exists.
#                 if it does not, it creats an atom at the point,and then calculates 
#                 the bond angle.
sub bondSetup
  {
    my $self=shift;
    my $event=shift;
    #start an undo container , the tags are reversed because this is a stack
    #check if there are any atoms at all
    my $p={x=>$self->canvasx($event->x),y=>$self->canvasy($event->y)};
    my @atoms=$self->{ChemCanvasMol}->atoms();
    if(scalar @atoms)
      {
	#find the closest atom
	#there are some atoms. lets find the closest one
	
	my $result=findNearestAtom($p,\@atoms);
	    $self->{ChemCanvasBondOrigin}=$result->{coord};
	    $self->{ChemCanvasBondAtom1}=$result->{atom}->id();
	    my @bonds=$result->{atom}->bonds();
	    if(@bonds)
	      {
		my $bond=pop @bonds;
		my @atoms=$bond->atoms();
		my $a1coord=$atoms[0]->coords();
		my $a2coord=$atoms[1]->coords();
		my $x=$a1coord->x -$a2coord->x;
		my $y=$a1coord->y - $a2coord->y;
		$self->{ChemCanvasBondVector}=Math::VectorReal->new($x,$y,0);
	      }	
	    else
	      {
		$self->{ChemCanvasBondVector}=Math::VectorReal->new(1,0,0);
	      }
      }	
    else
      {
	#no atoms, we must make one
	my $id=$self->addAtom({x=>$self->canvasx($event->x),y=>$self->canvasy($event->y)});
	$self->{ChemCanvasBondOrigin}={x=>$self->canvasx($event->x),y=>$self->canvasy($event->y)};
	$self->{ChemCanvasBondAtom1}=$id;
	$self->{ChemCanvasBondVector}=Math::VectorReal->new(1,0,0);
      }
  }	
##### 
#  class helper method
# Arguments: degrees between bonds, must be a factor of 360
# Returns: nothing
# Exceptions none
# Additional Info creates an array of point hashes that represent possible bond endpoints
sub setUpBondAngles
  {
    my $self=shift;
    my $degrees=shift;
    my $steps=360/$degrees;
    my $rad=(2*3.14159)/(360/$degrees);
    my @endpoints;
    for(0 .. $steps)
      {
	my ($x,$y);
	$x=$self->{ChemCanvasBondOrigin}->{x}+cos($_*$rad)*$self->{ChemCanvasBondLength};
	$y=$self->{ChemCanvasBondOrigin}->{y}+sin($_*$rad)*$self->{ChemCanvasBondLength};
	push @endpoints,{x=>$x,y=>$y};
      }
    $self->{ChemCanvasBondEndpoints}=[@endpoints];
  }	
##### 
# private class method
# Arguments: XEvent
# Returns: nothing
# Exceptions none
# Additional Info called to start a bond
sub startBond
  {
    my $self=shift;
    my $event=shift;
    
    $self->bondSetup($event);

    $self->CanvasBind("<Motion>",\&drag);
    my $bondLength=$self->{ChemCanvasBondLength};
    #calculate the potential endpoints for the bond
    #at every 15 degre mark
    $self->setUpBondAngles(15);
    $self->{ChemCanvasCurrentBondEndpoint}=undef;
    $self->{ChemCanvasCurrentBondTag}=undef;
  }	

##### 
# private class  method
# Arguments: XEvent
# Returns: the created bond
# Exceptions none
# Additional Info called when the button is released in bond mode. creates an atom if necessary, and creates the bond
#                 impliments the single click bond alg
sub endBond
  {
    my $self=shift;
    my $event=shift;
    my $createdBond;
    if($self->{ChemCanvasManualSelect})
      {
	$self->itemconfigure($self->{ChemCanvasManualSelect},-fill=>"");
      }	
    $self->CanvasBind("<Motion>","");
    #if there is an atom nearby then bind those 2 else create a new atom
    my $p={x=>$self->canvasx($event->x),y=>$self->canvasy($event->y)};
    my @atoms=$self->{ChemCanvasMol}->atoms();
    my $result=findNearestAtom($self->{ChemCanvasCurrentBondEndpoint},\@atoms);
    my $id;
    if($result->{distance}<5)
      {
	$id=$result->{atom}->id();
      }
    else
      {
	#then there are no atoms near the endpoint
	#need to check if there are atoms near the mouse
	$result=findNearestAtom($p,\@atoms);
	if($result->{distance}<7)
	  {
	    $id=$result->{atom}->id();
	  }
	else
	  {
	    my $p=$self->{ChemCanvasCurrentBondEndpoint};
	    if($p)
	      {
		$id=$self->addAtom($self->{ChemCanvasCurrentBondEndpoint});
	      }
	    else
	      {
		$id=undef;
	      }	
	  }
      }
    #delete the temporary bond drawing
    $self->delete($self->{ChemCanvasCurrentBondTag});
    if($id)
      {
	#determine if these atoms are already bonded
	if($id eq $self->{ChemCanvasBondAtom1})
	  {
	    ### this implies a single click on a atom ...
	    my $atom=$self->{ChemCanvasMol}->by_id($id);
	    my @bonds=$atom->bonds();
	    my $bondCount=scalar @bonds;
	    ### $bondCount
	    if($bondCount== 0)#clicked atom bond count
	      {
		### clicked atom has 0 bonds ...
		my $bondEndpoint;
		my $atomLocation=$atom->coords();
		my $v=Math::VectorReal->new(1,0,0);
		$v=$v*$self->{ChemCanvasBondLength};
		$bondEndpoint=$v+$atomLocation;
		my $newId=$self->addAtom({x=>$bondEndpoint->x,y=>$bondEndpoint->y});
		$createdBond=$self->createBond($id,$newId,1);
	      }	
	    elsif($bondCount==1) #clicked atom bond count
	      {
		### clicked atom has one bond ...
		#find out how many bonds the other atom has
		my @atoms=$bonds[0]->atoms();
		my $otherAtom;
		foreach my $a(@atoms)
		  {
		    if ($a != $atom)
		      {
			#this is the other end
			$otherAtom=$a;
			last;
		      }
		  }	
		my @otherBonds=$otherAtom->bonds();
		#if other end has bonds
		my $otherBondCount=scalar @otherBonds;
		### $otherBondCount
		if($otherBondCount !=2)
		  {
		    my $existingBondAngle=$self->formalBondAngle($atom,$otherAtom);
		    ### $existingBondAngle
		    my $angle=$existingBondAngle+120;
		    my ($x,$y);
		    ### $angle
		    $y=cos(deg2rad($angle))*$self->{ChemCanvasBondLength}*-1;
		    ### $y
		    $x=sin(deg2rad($angle))*$self->{ChemCanvasBondLength};
		    ### $x
		    my $origin=$self->{ChemCanvasBondOrigin};
		    my $bondEndpoint={x=>$origin->{x}+$x,y=>$origin->{y}+$y};
		    my $newId=$self->addAtom($bondEndpoint);
		    $createdBond=$self->createBond($id,$newId,1);
		  }
		elsif($otherBondCount ==2)
		  {
		    ### look at the second bond
		    my $bond1Angle=$self->formalBondAngle($atom,$otherAtom);
		    ### $bond1Angle
		    #find the third bond angle
		    my $firstBond=$bonds[0];
		    my $secondBond;
		    foreach my $b(@otherBonds)
		      {
			if( $b != $firstBond)
			  {
			    $secondBond=$b;
			    last;
			  }	
		      }
		    my @secondAtoms=$secondBond->atoms();
		    my $secondAtom;
		    foreach my $a(@secondAtoms)
		      {
			if($a != $otherAtom)
			  {
			    $secondAtom=$a;
			    last;
			  }	
		      }	
		    my $bond2Angle=$self->formalBondAngle($otherAtom,$secondAtom);
		    ### $bond2Angle
		    my $angle=$bond2Angle + 180;
		    my ($x,$y);
		    ### $angle
		    $y=cos(deg2rad($angle))*$self->{ChemCanvasBondLength}*-1;
		    ### $y
		    $x=sin(deg2rad($angle))*$self->{ChemCanvasBondLength};
		    ### $x
		    my $origin=$self->{ChemCanvasBondOrigin};
		    my $bondEndpoint={x=>$origin->{x}+$x,y=>$origin->{y}+$y};
		    my $newId=$self->addAtom($bondEndpoint);
		    $createdBond=$self->createBond($id,$newId,1);
		  }	
	      }
	    elsif($bondCount>1)
	      {
		#find largest angle
		### more that 1 ...
		### $bondCount
		my @bondVectors;
		my $origin=$atom->coords();
		my @bonds=$atom->bonds();
		my @otherAtoms;
		my @allOtherAtoms;
		foreach my $b(@bonds)
		  {
		    push @allOtherAtoms,$b->atoms();
		  }
		foreach my $aoa(@allOtherAtoms)
		  {
		    if ($aoa != $atom)
		      {
			push @otherAtoms,$aoa;
		      }
		  }
		#now have a list of other atoms in the bond
		foreach my $oa (@otherAtoms)
		  {
		    my $v=$oa->coords-$origin;
		    my $formalAngle=$self->formalAngleVector($v);
		    push @bondVectors,{v=>$v,atom=>$oa,formalAngle=>$formalAngle};
		  }
#		debugprintbondvectors(@bondVectors);
		@bondVectors=sort{$a->{formalAngle}<=>$b->{formalAngle}} @bondVectors;
#		debugprintbondvectors(@bondVectors);
	        #pair wise find angles
		my $sum=0;
		my @bondAngles;
		for my $index(0 .. (scalar @bondVectors - 2))
		  {
		    ### $index
		    my $a2=$bondVectors[$index+1]->{formalAngle}- $bondVectors[$index]->{formalAngle};
		    #this subtraction works because these angles are already sorted in the clockwise dir
		    $sum=$sum+$a2;
		    push @bondAngles,{angle=>$a2,v1=>$bondVectors[$index],v2=>$bondVectors[$index+1]};
#		    push @bondAngles,{angle=>$angle,v1=>$bondVectors[$index],v2=>$bondVectors[$index+1]};
		  }	
		#final angle
		my $angle=360 - $sum;
		push @bondAngles,{angle=>$angle,v1=>$bondVectors[0],v2=>$bondVectors[-1]};
		@bondAngles= sort { $b->{angle} <=> $a->{angle}} @bondAngles;
		my $largest=$bondAngles[0];
		my $largestAngle=nearest(1,$largest->{angle});
		my $newAngle=$largest->{angle}/2;
		### $largestAngle
		### $newAngle
		#find the formal angle of each bond
		my $formalAngle1=$self->formalBondAngle($atom,$largest->{v1}->{atom});
		my $formalAngle2=$self->formalBondAngle($atom,$largest->{v2}->{atom});
		### $formalAngle1
		### $formalAngle2
		#take the smallest of the 2
		my ($startAngle,$endAngle);
		if($formalAngle1 < $formalAngle2)
		  {
		    $startAngle=$formalAngle1;
		    $endAngle=$formalAngle2;
		  }	
		else 
		  {
		    $startAngle=$formalAngle2;
		    $endAngle=$formalAngle1;
		  }	
		### $startAngle
		### $endAngle
		#want the new angle to be between the 2 angles
		my $angle;
		my $sToEAngle=nearest(1,($endAngle-$startAngle));
		if($sToEAngle != $largestAngle)
		  {#go counter clk wise
		    $angle=$startAngle-$newAngle;
		  }	
		else
		  {
		    $angle=$startAngle+$newAngle;
		  }
		### $sToEAngle
		my $y=cos(deg2rad($angle))*$self->{ChemCanvasBondLength}*-1;
		my $x=sin(deg2rad($angle))*$self->{ChemCanvasBondLength};
		my $origin=$self->{ChemCanvasBondOrigin};
		my $bondEndpoint={x=>$origin->{x}+$x,y=>$origin->{y}+$y};
		my $newId=$self->addAtom($bondEndpoint);
		$createdBond=$self->createBond($id,$newId,1);
	      }
	  }
	else
	  {
	    my @bonds1=$self->{ChemCanvasMol}->by_id($id)->bonds();
	    my @bonds2=$self->{ChemCanvasMol}->by_id($self->{ChemCanvasBondAtom1})->bonds();
	    my %set;
	    foreach(@bonds1)
	      {
		$set{$_->id()}=1;
	      }	
	    foreach(@bonds2)
	      {
		$set{$_->id()}=$set{$_}+1;
	      }	
	    my @sharedBonds;
	    foreach my $key(keys %set)
	      {
		if ($set{$key} ==2)
		  {
		    push @sharedBonds,$key;
		  }
	      }	
	    if(@sharedBonds)
	      {
		my $bondID=$sharedBonds[0];#any two atoms should share only one bond
		my $order=$self->{ChemCanvasMol}->by_id($bondID)->order();
		$self->changeBondOrder($bondID);
	      }
	    else
	      {
		$createdBond=$self->createBond($self->{ChemCanvasBondAtom1},$id,1);
	      }	
	  }
      }
    
    $self->delete("angle");
    return $createdBond;
  }
##### 
# class helper  method
# Arguments: first atom the origin of the bond, second atom, the endpoint of a bond
# Returns: returns the 360 degree angle with 0 at north
# Exceptions
# Additional Info complications arise because acos only retruns angles 0<angle<180
sub formalBondAngle
  {
    my $self=shift;
    my $atom=shift;
    my $otherAtom=shift;
    my $dir=$self->direction($atom->coords(),$otherAtom->coords());
    my $angle;
    my $v;
    $v=$otherAtom->coords-$atom->coords;
    my $existingBondAngle=$self->measureAngle($v,Math::VectorReal->new(0,-1,0));
    if($dir eq "left")
      {
	if ($existingBondAngle < 90)
	  {
	    $existingBondAngle=360-$existingBondAngle;
	  }
	else
	  {
	    my $diff=180-$existingBondAngle;
	    $existingBondAngle=180+$diff;
	  }
      }

#    print "formal bond angle $existingBondAngle\n";
    return round($existingBondAngle);
  }
##### 
# class helper method
# Arguments: a vector
# Returns: the 360 angle with 0 at north
# Exceptions none
# Additional Info complications arise becaus acos only return 0<angle<180
sub formalAngleVector
  {
    ### formalAngleVector ...
    my $self=shift;
    my $v=shift;
    ### $v
    my $dir=$self->direction(Math::VectorReal->new(0,-1,0),$v);
    my $angle=$self->measureAngle($v,Math::VectorReal->new(0,-1,0));
    if($dir eq "left")
      {
	if ($angle < 90)
	  {
	    $angle=360-$angle;
	  }
	else
	  {
	    my $diff=180-$angle;
	    $angle=180+$diff;
	  }
      }
    ### $angle
    return $angle;
  }
##### 
#  class helper method
# Arguments: 2 Math::VectorReal that represent 2 points
# Returns:  left or right
# Exceptions none
# Additional Info determines if the second point is to the left or right of the first point
sub direction
  {
    my $self=shift;
    my $v1=shift;
    my $v2=shift;
    my $dir;
    my $bondVec=$v1-$v2;
        my $dir;
    my $ax=nearest(0.1,$v1->x);
    my $ay=nearest(0.1,$v1->y);
    my $ox=nearest(0.1,$v2->x);
    my $oy=nearest(0.1,$v2->y);
    if($ax > $ox)
      {
	$dir="left";
      }	
    elsif( $ax==$ox)
      {
	$dir="right";
      }	
    else
      {
	$dir="right";
      }	
    return $dir;
  }
##### 
# class helper  method
# Arguments: 2 Math::VectorReal obj
# Returns: the relative angle between them 
# Exceptions none
# Additional Info the angle is always less than or equal to 180
sub measureAngle
  {
    
    my $self=shift;
    my $v1=shift;
    my $v2=shift;
    
    $v1=$v1->norm;
    $v2=$v2->norm;
    my $val=$v1.$v2;
    my $angle=rad2deg(acos($val));
    return round($angle);
  }	
##### 
# private class method
# Arguments: xevent
# Returns: nothing
# Exceptions none
# Additional Info draw a bond from the bond origin to the nearest bond endpoint that was calculated
#                 in the bondsetup
sub drag
  {
    my $self=shift;
    my $event= $self->XEvent;
    my $bondLength=$self->{ChemCanvasBondLength};
    my $distance;
    my ($x,$y,$originX,$originY);
    $self->delete("angle");
    $x=$self->canvasx($event->x);
    $y=$self->canvasy($event->y);
    $originX=$self->{ChemCanvasBondOrigin}->{x};
    $originY=$self->{ChemCanvasBondOrigin}->{y};
    $distance=sqrt(($x-$originX)**2+($y-$originY)**2);
    my $condition;
    if($self->{ChemCanvasMode} eq "bonds")
      {
	$condition=$distance> .5*$bondLength;
      }	
    if($condition)
      {
	#if the mouse has moved more that half the bond length
	#find the closest endpoint and draw the bond
	my @endpoints=@{$self->{ChemCanvasBondEndpoints}};
	my $minEndpoint;
	my $minDistance=1000000000;
	foreach my $endpoint(@endpoints)
	  {
	    $distance=distance($endpoint,{x=>$x,y=>$y});
	    if($distance<$minDistance)
	      {
		$minDistance=$distance;
		$minEndpoint=$endpoint;
	      }
	  }	
	my $currEndpoint=$self->{ChemCanvasCurrentBondEndpoint};
	my $tag;
	my $v=Math::VectorReal->new($originX-$minEndpoint->{x},$originY-$minEndpoint->{y},0);
	
	my $angle=calcAngle($self->{ChemCanvasBondVector},$v);
	#$self->createText($originX+20,$originY-20,-text=>$angle,-tags=>"angle"); #uncomment to display the angle of the bond
	if ($currEndpoint)#the endpoint has been set once
	  {
	    #check if the new endpoint is different
	    if(($currEndpoint->{x} != $minEndpoint->{x}) or ($currEndpoint->{y} != $minEndpoint->{y}))
	      {
		#need to delete the current drawing
		$self->delete($self->{ChemCanvasCurrentBondTag});
		$self->createLine($originX,$originY,$minEndpoint->{x},$minEndpoint->{y},-tags=>$self->{ChemCanvasCurrentBondTag});
		$self->{ChemCanvasCurrentBondEndpoint}=$minEndpoint;
	      }
	  }	
	else
	  {
	    $tag="bond".$self->getIndex();
	    $self->createLine($originX,$originY,$minEndpoint->{x},$minEndpoint->{y},-tags=>$tag);
	    $self->{ChemCanvasCurrentBondEndpoint}=$minEndpoint;
	    $self->{ChemCanvasCurrentBondTag}=$tag;
	    $self->{ChemCanvasCurrentBondEndpoint}=$minEndpoint;
	  }
      }
    my @atoms=$self->{ChemCanvasMol}->atoms();
    my $nearest=findNearestAtom({x=>$x,y=>$y},\@atoms);
    if($self->{ChemCanvasManualSelect})
      {
	$self->itemconfigure($self->{ChemCanvasManualSelect},-fill=>"");
      }	
    if($nearest->{distance}< 5)
      {
	$self->itemconfigure($nearest->{atom}->id(),-fill=>"blue");
	$self->{ChemCanvasManualSelect}=$nearest->{atom}->id();
	$self->delete($self->{ChemCanvasCurrentBondTag});
	$self->createLine($originX,$originY,$x,$y,-tags=>$self->{ChemCanvasCurrentBondTag});
      }	
  }
##### 
# private class method
# Arguments:  2 atoms
# Returns: the created bond
# Exceptions none
# Additional Info creates a bond between the arguments
sub createBond
  {
    my $self=shift;
    my $firstAtomId=shift;
    my $secondAtomId=shift;
    my $order=shift;
    my $isUndo=shift;
    my $a1=$self->{ChemCanvasMol}->by_id($firstAtomId);
    my $a2=$self->{ChemCanvasMol}->by_id($secondAtomId);
    my $bond=$self->{ChemCanvasMol}->new_bond(atoms=>[$a1,$a2],order=>$order);
    $self->drawBond($bond);
    my $coords=$a1->coords();
    $self->drawAtom($a1->id(),{x=>$coords->x,y=>$coords->y},$a1);
    $coords=$a2->coords();
    $self->drawAtom($a2->id(),{x=>$coords->x,y=>$coords->y},$a2);
    my $undoRecord={op=>"deleteBond",id=>$bond->id(),order=>$order};
    push @{$self->{ChemCanvasUndoStack}},$undoRecord unless $isUndo;
    return $bond;
  }	
##### 
# private class  method
# Arguments: a bond id
# Returns: nothing
# Exceptions none
# Additional Info changes the bond order from 1->2->3->1...
sub changeBondOrder
  {
    my $self=shift;
    ### @_
    my $tag=shift;
    my $newOrder=shift;
    my $bond=$self->{ChemCanvasMol}->by_id($tag);
    return unless $bond->isa("Chemistry::Bond");
    my $order;
    my $oldOrder;
    if (!$newOrder)
      {
	$order = $bond->order();
	$oldOrder=$order;
	if ($order == 1)
	  {$order=2; }
	elsif($order ==2)
	  {$order=3;}	
	elsif($order == 3)
	  {$order=1;}
	else
	  {$order=1;}
      }
    else
      {
	$order=$newOrder;
      }	
    ### $order
    ### $oldOrder
    $bond->order($order);
    $self->delete($tag);
    $self->drawBond($bond);
    my @atoms=$bond->atoms();
    foreach my $a(@atoms)
      {
	$self->drawAtom($a->id(),$a->coords(),$a);
      }	
    push @{$self->{ChemCanvasUndoStack}},{op=>"changeBondOrder",id=>$tag,order=>$oldOrder} unless $newOrder;
  }	
##### 
# private class method
# Arguments: a bond id
# Returns: nothing
# Exceptions none
# Additional Info draws the bond on the canvas
sub drawBond
  {
    my $self=shift;
    my $bond=shift;
    my $tag=$bond->id();
    my @atoms=$bond->atoms();
    
    my $order=$bond->order();
    $self->delete($tag);
    unless($atoms[0])
      {
	return ;
      }
    my $start=$atoms[0]->coords();
    my $end =$atoms[1]->coords();
    #create a perpendicular unit vector
    my ($vec,$x,$y,$temp);
    $vec=$atoms[0]->coords()-$atoms[1]->coords();
    $vec=$vec->norm;
    $x=$vec->x;
    $y=$vec->y;
    $temp=$x;
    $x=-1*$y;
    $y=$temp;
    my $color;
    if($bond->attr("attachment"))
      {
	$color="red";
      }	
    else{$color="black";}
    
    if($order == 1)
      {
	$self->createLine($start->x,$start->y,$end->x,$end->y,-tags=>$tag,-fill=>$color);
      }
    elsif($order == 2)
      {
	$self->createLine($start->x-2*$x,$start->y-2*$y,$end->x-2*$x,$end->y-2*$y,-tags=>$tag,-fill=>$color);
	$self->createLine($start->x+2*$x,$start->y+2*$y,$end->x+2*$x,$end->y+2*$y,-tags=>$tag,-fill=>$color);
	
      }	
    elsif($order==3)
      {
	$self->createLine($start->x-2*$x,$start->y-2*$y,$end->x-2*$x,$end->y-2*$y,-tags=>$tag,-fill=>$color);
	$self->createLine($start->x,$start->y,$end->x,$end->y,-tags=>$tag,-fill=>$color);
	$self->createLine($start->x+2*$x,$start->y+2*$y,$end->x+2*$x,$end->y+2*$y,-tags=>$tag,-fill=>$color);
      }	
    else
      {
	$self->createLine($start->x,$start->y,$end->x,$end->y,-tags=>$tag,-fill=>$color);
      }
    $self->bind($tag,"<Enter>",[\&highlightBond,$tag]);
    $self->bind($tag,"<Leave>",[\&unHighlightBond,$tag]);
    
  }
##### 
# class helper method
# Arguments: a bond tag
# Returns: nothing
# Exceptions none
# Additional Info bound to the <enter> event on a bond, used to visually indicate that the bond is selected
sub highlightBond
  {
    my $self=shift;
    my $tag=shift;
    $self->itemconfigure($tag,-width=>4);
  }
##### 
# class helper method
# Arguments: a bond tag
# Returns: nothing
# Exceptions none
# Additional Info bound to the <leave> event on a bond, used to visually indicate that the bond in not selected
sub unHighlightBond
  {
    my $self=shift;
    my $tag=shift;
    $self->itemconfigure($tag,-width=>1);
  }	
##### 
# public class accessor method
# Arguments: a length
# Returns: the new length
# Exceptions none
# Additional Info
sub bondLength
  {
    my $self=shift;
    if(@_)
      {
	$self->{ChemCanvasBondLength}=shift;
	return $self->{ChemCanvasBondLength};	
      }	
    else
      {
	return $self->{ChemCanvasBondLength};
      }
  }
##### 
# public class  method
# Arguments: none
# Returns:none
# Exceptions none
# Additional Info erases everyting on the canvas, and creates a new empty molecule
sub clear
  {
    my $self=shift;
    $self->delete("all");
    $self->{ChemCanvasMol}=Chemistry::Mol->new(id => "ChemCanvasmol", );
  }
##### 
# public class accessor method
# Arguments: a Chemistry::Mol obj
# Returns: a Chemistry::Mol obj
# Exceptions croaks if the arg is not a chemistry::mol
# Additional Info 
sub molecule
  {
    my $self=shift;
    if(@_)
      {
	$self->delete("all");
	my $m=shift;
	croak("The molecule pass in is not a Chemistry::mol object") unless $m->isa("Chemistry::Mol");
	$m=$m->safe_clone();
	$m=$self->normalize($m,$self->{ChemCanvasBondLength});
	$self->{ChemCanvasMol}=$m;
	$self->drawMolecule($self->{ChemCanvasMol});
      }	
    else
      {
	my $m=$self->{ChemCanvasMol}->safe_clone();
	$m=$self->normalize($m,1);
	return $m;
      }	
  }	
##### 
# private class  method
# Arguments: a molecule
# Returns: nothing
# Exceptions none
# Additional Info draws the arg on the canvas
sub drawMolecule
  {
    my $self=shift;
    my $m=shift;
    my @bonds=$m->bonds();
    foreach my $b(@bonds)
      {
	$self->drawBond($b);
      }	
    my @atoms=$m->atoms();
    foreach my $a(@atoms)
      {
	my $tag=$a->id();
	my $p=$a->coords();
	$self->drawAtom($tag,$p,$a);
      }	

  }	
##### 
# public class accessor method
# Arguments: a chemistry::mol object
# Returns: a chemistry mol object
# Exceptions croaks if the arg in not a chemistry::mol
# Additional Info
sub template
  {
    my $self=shift;
    if(@_)
      {
	my $m=shift;
	croak("The template is not a Chemistry::Mol") unless $m->isa("Chemistry::Mol");
	$m=$m->safe_clone;
	$m=$self->translateAttachment($m,Math::VectorReal->new(0,0,0));
	$m=$self->normalize($m,$self->{ChemCanvasBondLength});
	$self->{ChemCanvasTemplate}=$m;
      }
    else
      {
	my $m= $self->{ChemCanvasTemplate}->safe_clone;
	$m=$self->normalize($m,1);
	return  $m;
      }
  }	
##### 
# class helper method
# Arguments: a molecule, and a point hash
# Returns: the translated molecule
# Exceptions none
# Additional Info translates the molecules attachment atom to the point hash
sub translateAttachment
  {
    my $self=shift;
    my $m=shift;
    my $p=shift;
    #find the atom with the attachment attr set
    #failing that pick the first atom
    my @atoms=$m->atoms();
    my $attach=$atoms[0];
    foreach my $a(@atoms)
      {
	if( $a->attr("attachment"))
	  {
	    $attach=$a;
	  }	
      }	
    my $v=$attach->coords();
    #$p=$v-x
    #-x=$p-$v
    #x=v-$p;
    my $x=$v-$p;
    #move the attachment atom to $p

    foreach my $a(@atoms)
      {
	$a->coords($a->coords()-$x);
      }	

    return $m;
  }	
##### 
# class helper  method
# Arguments: a molecule and a Math::VectorReal obj
# Returns: the molecule
# Exceptions none 
# Additional Info moves the molecule by the vector arg
sub translate
  {
    
    my $self=shift;
    my $m=shift;
    my $p=shift;
    my @atoms=$m->atoms();
    foreach my $a(@atoms)
      {
	$a->coords($a->coords()-$p);
      }	


    return $m;
  }	
##### 
# class helper  method
# Arguments: Chemistry::mol obj
# Returns: normalized Chemistry::Mol obj
# Exceptions
# Additional Info makes a histogram of bond length in $m, then takes the largest bin
#                 and calculates the scale factor to scale the largest bin to the current
#                 bond length, then scales all of the bonds by that length
sub normalize
  {
    my $self=shift;
    my $m=shift;
    my $targetLength=shift;
    my @bonds=$m->bonds();
    my %bonds;
    #find the largest mode of bond lengths and take that as the standard
    foreach my $b(@bonds)
      {
	my $l=$b->length();
	$l=round($l);
	$bonds{$l}=$bonds{$l}+1;
      }	
    my $largest=0;
    my $length=0;
    foreach my $k(keys %bonds)
      {
	if($bonds{$k} > $largest)
	  {
	    $largest=$bonds{$k};
	    $length=$k;
	  }	
      }

    #my $targetLength=$self->{ChemCanvasBondLength};
    my $ratio=$targetLength/$length;
    my @atoms=$m->atoms();
    foreach my $a(@atoms)
      {
	my $v=$a->coords();
	$v=$v*$ratio;
	$a->coords($v);
      }	
    return $m;
  }	
##### 
# class helper method
# Arguments: an angle, a molecule
# Returns: the molecule
# Exceptions none
# Additional Info rotates the template by angle. the atachment atom should be at 0,0
sub rotate
  {
    my $self=shift;
    my $angle=shift;
    my $template=shift;
    
    my @atoms=$template->atoms();
    foreach(@atoms)
      {
	my $coords=$_->coords();
	my $xprime=$coords->x*cos(deg2rad($angle)) - $coords->y*sin(deg2rad($angle));
	my $yprime=$coords->x*sin(deg2rad($angle))+ $coords->y*cos(deg2rad($angle));
	$_->coords(Math::VectorReal->new($xprime,$yprime,0));
      }	
    return $template;
  }
##### 
# helper method
# Arguments: 2 point hashes, or 2 Math::VectorReal objects
# Returns: the distance between them
# Exceptions
# Additional Info
sub distance
  {
    my $p1=shift;
    my $p2=shift;
    if(ref $p1 eq ref Math::VectorReal->new(0,0,0))
      {
	return sqrt(($p1->x-$p2->x)**2+($p1->y-$p2->y)**2);
	
      }	
    return sqrt(($p1->{x}-$p2->{x})**2+($p1->{y}-$p2->{y})**2);
  }	
##### 
# helper method
# Arguments: a point hash, an aray ref to a list of atoms 
# Returns: a hash with 3 keys
#               distance => the distance to the point from the nearest atom
#               coord=> a point hash of the nearest atom coords
#               atom=> the nearest atom
# Exceptions
# Additional Info
sub findNearestAtom
  {
    my $p=shift;
    my $atoms=shift;
    my @atoms=@{$atoms};
    if( scalar @atoms)
      {
	my ($minDistance,$minOrigin,$a);
	$minDistance=100000000000;
	foreach my $atom(@atoms)
	  {
	    my $coords=$atom->coords();
	    #$coords is a Math::VectorReal obj
	    my $distance=distance({x=>$p->{x},y=>$p->{y}},{x=>$coords->x,y=>$coords->y});
	    if($distance< $minDistance)
	      {
		$minDistance=$distance;
		$minOrigin={x=>$coords->x,y=>$coords->y};
		$a=$atom;
	      }	
	  }
	return {coord=>$minOrigin,distance=>$minDistance,atom=>$a};
      }
    return undef;
  }
##### 
#  helper method
# Arguments: 2 Math::VectorReal
# Returns: the angle between them 
# Exceptions 
# Additional Info angle will be limited to 0<angle<180
sub calcAngle
  {
    my $v1=shift;
    my $v2=shift;
    $v1=$v1->norm();
    $v2=$v2->norm();
    my $dot=$v1.$v2;
    my $angleRad= acos($dot);
    my $angle=($angleRad *180)/3.14159;
    return round($angle);
  }
sub debugMol
  {
    my $m=shift;
    my @keys=keys %{$m->{byId}};
    print @keys;
  }	
sub printUndoStack
  {
    my $self=shift;
    my $undo=$self->{ChemCanvasUndoStack};
    foreach my $undoOp(@{$undo})
      {
	print "op ",$undoOp->{op},"\n";
      }	
  }	
sub debugprintbondvectors
  {
    my @bV=@_;
    my $i=0;
    foreach my $v(@bV)
      {
	print "bondVector $i:\n";
	print "\t",$v->{formalAngle},"\n";
      }	
  }
__END__
#some data structures used
#
# point hash
# a hash ref with 2 keys
#     x => a numeric value
#     y => a numeric value
#   represents a pint in cartesian space

#keys used in the ChemCanvas
# all keys are prefixed with ChemCanvas to prevent any name collisions
# but for these definitions the ChemCanvas will be dropped
#
#  Mode - the current mode of the canvas
#  CurrentAtom - the periodic table symbol of the atom that will be placed or changed.
#  Index - an integer used to make ids for atoms and bonds
#  Mol - a Chemistry::Mol object , this is the molecule displayed by the canvas
#  UndoStack - an array ref of undo operations.
#  BondLength - the current bond length in pixels
#  CurrentBondEndpoint - Where the current bond ends durring a drag operation , a point hash
#  CurrentBondTag - the tag for the current bond
#  BondOrigin -  where the current bond starts, used durring a drag operation
#  BondEndpoints - a array ref of where the current bond could end. the bond snaps to these points durring a drag
#  ChainAtomLocations - a array ref of atom locations. used durring a chain drag. these atoms do not exist, but will be created and bound when the drag completes
#  BondAtom1 - the first atom in a bond, used durring bond creation
#  BondVector - a Math::VectorReal that points in the direction of the bond. used to display the bond angele
#  Template - a chemistry::Mol that represents the template to be placed in template mode. may have attach attributes on one atom and/or one bond
#  ManualSelect - stores the atom id for atoms selected durring bond creation drag.

 ##############################################################################
 ##    Example 7.1 (Recommended) from Chapter 7 of "Perl Best Practices"     ##
 ##     Copyright (c) O'Reilly & Associates, 2005. All Rights Reserved.      ##
 ##  See: http://www.oreilly.com/pub/a/oreilly/ask_tim/2001/codepolicy.html  ##
 ##############################################################################


#  Example 7-1. User documentation template for modules

=head1 ChemCanvas
 
Tk::ChemCanvas - Easily manipulate 2d molecules with Tk

This documentation refers to TK::ChemCanvas version 1.0.0


=head1 SYNOPSIS

    use Tk::ChemCanvas;
    my $mw=MainWindow->new();
    my $c=$mw->ChemCanvas->pack();


=head1 DESCRIPTION

ChemCanvas is a widget derived from the Canvas widget.  All of the Canvas functionality 
of the Canvas is present with added functionality to facilitate displaying and maniipulating
molecules.


=head1 SUBROUTINES/METHODS 

This section describes the public API of the ChemCanvas

=head3  mode()


Sets or gets the current mode for the ChemCanvas.

$self->mode({-mode=>I<<mode>>});

Valid modes are :

=over

=item * elements

=item * bonds 

=item * erase

=item * chain

=item * template

=back

The mode controls what happens when the user clicks on the canvas area. 

=head4 elements

In elements mode a single click places an atom of the current atom type or changes 
the type of the atom clicked on.

=head4 bonds

This mode creates bonds between atoms. There are 2 ways to create a bond, single click, or click and drag.
The single click will create the first atom if there are no atoms. When there are atoms, the click will choose the 
nearest atom and create a single bond to a new atom at a reasonable angle.
Click and drag will create an atom at the drag angle, and that angle will continue to adjust until the drag ends.
A bond's order can be changed by clicking on the bond, or draging another bond between the two atoms.

=head4 erase

As indicated when an atom or bond is clicked in erase mode, that bond or atom is erased. orphan atoms are also deleted.

=head4 chain

Chain mode creates chains of atoms at alternating 120 degree angles. The chain snaps to 60 degree angles that follow the drag.

=head4 template

Template mode places the current template at the bond, or atom clicked on.

=head3 currentAtom

$self->currentAtom();

$self->currentAtom("C");

currentAtom sets or gets the current atom chemical symbol.

=head3 undo

$self->undo();

Undo reverts the last change to the molecule. This function can be called repeatedly to undo several changes.

=head3 bondLength

$self->bondLength();

$self->bondLength(50);

Sets (or gets) the bond length in pixels.

=head3 clear

$self->clear();

Clear deletes the current molecule completely. Undo is not supported.

=head3 molecule

$self->molecule();

$self->molecule($m);

Gets or sets the current molecule. $m must be a Chemistry::mol. The molecule will be safe_cloned.

=head3 template

$self->template();

$self->template($t);

Sets or gets the current template. $t must be a Chemistry::mol object.  $t should have either the attachment bond or atom set, or both set.
A right click on a bond marks it as the attachment bond or atom. $t will be safe_cloned.

To programaticly specify the attachment atom or bond, simply add an attachment attribute.

$atom->attr("attachment","atom");

$bond->attr("attachment","bond");


=head1 DEPENDENCIES

This module depends on Tk, obviously.

This module depends on the Chemistry::* modules version (0.36+)

This module depends on Math::VectorReal.

This module depends on Math::Trig.

This module depends on Math::Round.


=head1 BUGS AND LIMITATIONS

There are no known bugs in this module. 
Please report problems to <Kip...>  (...)
Patches are welcome.

=head1 AUTHOR

Dean Brockhausen


=head1 LICENCE AND COPYRIGHT

Copyright (c)2005  Dean Brockhausen  All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 


=cut


 #method comment template
 ##### 
 #  method
 # Arguments: 
 # Returns:
 # Exceptions
 # Additional Info
