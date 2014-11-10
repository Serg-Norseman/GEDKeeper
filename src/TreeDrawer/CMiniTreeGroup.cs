/* CMiniTreeGroup.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System.Collections;
using GEDmill.LLClasses;
using System.Drawing;

namespace GEDmill.ImageClasses
{
  // Represents a group of individuals in the tree, e.g. a group of siblings, that need to be kept together
  public class CMiniTreeGroup : CMiniTreeObject
  {
    // Type of gedcomLine joining this box horizontally to indicate marriage or other parenting relationship.
    public enum ECrossbar 
    {
      Solid = 0,
      DottedLeft = 1,
      DottedRight = 2
    };

    // The CMiniTreeIndividual boxes and groups that make up this group.
    private ArrayList m_alMembers;

    // Width of the gedcomLine joining this group to group above.
    private const float TEE_WIDTH = 0.0f;

    // Height of the gedcomLine joining this group to group above.
    private const float TEE_HEIGHT = 16.0f;

    // The screen size of this group
    private SizeF m_size;

    // The group that this group belongs to.
    private CMiniTreeGroup m_mtgParent;
  
    // The number of individuals in this group (as opposed to groups)
    private uint m_uIndividuals;

    // Number of individuals in this group with lines up to the generation above.
    private uint m_uStalkedIndividuals;

    // The left-most box.
    private CMiniTreeIndividual m_mtiBoxLeft;

    // The right-most box.
    private CMiniTreeIndividual m_mtiBoxRight;

    // Reference to the last box to be added to this group
    private CMiniTreeObject m_mtoLastAddedObject;

    // Type of gedcomLine joining this box horizontally to indicate marriage or other parenting relationship.
    public ECrossbar m_ecCrossbar;

    // Constructor
    public CMiniTreeGroup()
    {     
      m_alMembers = null;
      m_size = new SizeF( 0.0f, 0.0f );
      m_mtgParent = null;
      m_uIndividuals = 0;
      m_uStalkedIndividuals = 0;
      m_mtiBoxLeft = null;
      m_mtiBoxRight = null;
      m_mtoLastAddedObject = null;
      m_ecCrossbar = ECrossbar.Solid;
    }

    // Creates a CMiniTreeIndividual for the individual specified and adds it to the group.
    // Informs neighbouring boxes about this box.
    // bCreateLink decides whether to make this box a clickable link in the HTML.
    public CMiniTreeIndividual AddIndividual( CIndividualRecord ir, string sFirstnames, string sSurname, string sDate, bool bCreateLink, bool bCreateStalk, bool bHighlight, bool bConcealed, bool bShade )
    {
      CMiniTreeIndividual mti = new CMiniTreeIndividual( ir, sFirstnames, sSurname, sDate, bCreateLink, bCreateStalk, bHighlight, bConcealed, bShade, MainForm.s_config.m_bConserveTreeWidth );

      if( m_alMembers == null )
      {
        m_alMembers = new ArrayList();
      }
      m_alMembers.Add( mti );

      m_uIndividuals++;

      if( bCreateStalk )
      {
        m_uStalkedIndividuals++;
      }

      mti.LeftObject = m_mtoLastAddedObject;

      if( m_mtoLastAddedObject != null )
      {
        m_mtoLastAddedObject.RightObject = mti;
      }

      m_mtoLastAddedObject = mti;

      return mti;
    }

    // Adds a  CMiniTreeGroup to this group.
    // Informs neighbouring boxes about the group.
    public void AddGroup( CMiniTreeGroup mtg )
    {
      if( mtg != null )
      {
        if( m_alMembers == null )
        {
          m_alMembers = new ArrayList();
        }
        m_alMembers.Add( mtg );

        mtg.m_mtgParent = this;

        mtg.LeftObject = m_mtoLastAddedObject;

        if( m_mtoLastAddedObject != null )
        {
          m_mtoLastAddedObject.RightObject = mtg;
        }

        m_mtoLastAddedObject = mtg;
      }
    }

    // Calculates the size required by this group. Initialises the class fields 
    // that contain size information. Returns the overall group size.
    public override SizeF CalculateSize( Graphics g, Font f )
    {
      m_size.Width = 0.0f;
      m_size.Height = 0.0f;

      if( m_alMembers == null )
      {
        // Empty group
        return m_size;
      }

      foreach( object obj in m_alMembers )
      {
        SizeF size;
        if( obj is CMiniTreeIndividual )
        {
          size = ((CMiniTreeIndividual)obj).CalculateSize( g, f );
        }
        else if( obj is CMiniTreeGroup )
        {
          // Let group calculate its size for later
          ((CMiniTreeGroup)obj).CalculateSize( g, f );

          // Size here is only size of tee
          size = new SizeF( TEE_WIDTH, TEE_HEIGHT );
        }
        else
        {
          size = new SizeF( 0f, 0f );
        }

        m_size.Width += size.Width;
        if( size.Height > m_size.Height )
        {
          m_size.Height = size.Height;
        }
      }

      if( m_uIndividuals == 0 )
      {
        // Don't include tee size if no individuals
        m_size.Width = 0f;
        m_size.Height = 0f;
      }

      return m_size;
    }




    // Draws the group to the graphics instance.
    public override void DrawBitmap( CPaintbox paintbox, Graphics g, ArrayList alMap )
    {
      if( m_alMembers == null )
      {
        // Empty group
        return;
      }

      foreach( object obj in m_alMembers )
      {
        if( obj is CMiniTreeGroup )
        {
          if( ((CMiniTreeGroup)obj).m_mtiBoxLeft != null && ((CMiniTreeGroup)obj).m_mtiBoxRight != null )
          {
            // Draw crossbar
            float fCrossbarLeft = ((CMiniTreeGroup)obj).m_mtiBoxLeft.TeeRight;
            float fCrossbarRight = ((CMiniTreeGroup)obj).m_mtiBoxRight.TeeLeft;
            float fCrossbarLeftGap = ((CMiniTreeGroup)obj).m_mtiBoxLeft.Right;
            float fCrossbarRightGap = ((CMiniTreeGroup)obj).m_mtiBoxRight.Left;
            float fCrossbarY = (((CMiniTreeGroup)obj).m_mtiBoxLeft.TeeCentreVert + ((CMiniTreeGroup)obj).m_mtiBoxRight.TeeCentreVert) / 2f;
            switch( ((CMiniTreeGroup)obj).m_ecCrossbar )
            {
              case ECrossbar.Solid:
                g.DrawLine( paintbox.m_penConnector, fCrossbarLeft, fCrossbarY, fCrossbarRight, fCrossbarY );
                break;
                
              case ECrossbar.DottedLeft:
                g.DrawLine( paintbox.m_penConnectorDotted, fCrossbarLeft, fCrossbarY, fCrossbarRightGap, fCrossbarY );
                break;

              case ECrossbar.DottedRight:
                g.DrawLine( paintbox.m_penConnectorDotted, fCrossbarLeftGap, fCrossbarY, fCrossbarRight, fCrossbarY );
                break;

                default:
                    break;
            }

            if( ((CMiniTreeGroup)obj).m_uStalkedIndividuals > 0 )
            {
              // Draw down to individuals
              // Use y coord of first individual, assuming all are at the same y coord
              float fIndividualY = 0f;
              bool bHaveIndividuals = false;
              foreach( CMiniTreeObject groupObj in ((CMiniTreeGroup)obj).m_alMembers )
              {
                if( groupObj is CMiniTreeIndividual )
                {
                  fIndividualY = ((CMiniTreeIndividual)groupObj).Top;
                  bHaveIndividuals = true;
                  break;
                }
              }
              float fCrossbarCentre = (fCrossbarLeft+fCrossbarRight)/2f;
              if( bHaveIndividuals )
              {
                g.DrawLine( paintbox.m_penConnector, fCrossbarCentre, fCrossbarY, fCrossbarCentre, fIndividualY );

                // Connect individuals
                SizeF stalkMinMax = ((CMiniTreeGroup)obj).StalkMinMax;

                // Width irrelevant, using SizeF simply as a way to pass 2 floats:
                float fStalkMin = stalkMinMax.Width; 

                // Height irrelevant, using SizeF simply as a way to pass 2 floats
                float fStalkMax = stalkMinMax.Height; 

                if( fCrossbarCentre < fStalkMin )
                {
                  fStalkMin = fCrossbarCentre;
                }
                else if( fCrossbarCentre > fStalkMax )
                {
                  fStalkMax = fCrossbarCentre;
                }
                g.DrawLine( paintbox.m_penConnector, fStalkMin, fIndividualY, fStalkMax, fIndividualY );
              }
            }
          }

          ((CMiniTreeGroup)obj).DrawBitmap( paintbox, g, alMap );
        }
        else if( obj is CMiniTreeIndividual )
        {
          // Draw individual box
          ((CMiniTreeIndividual)obj).DrawBitmap( paintbox, g, alMap );
        }
      }
    }

    // Returns the size occupied by all the boxes in this group and its sub groups.
    // Caller must ensure members != null otherwise they will get returned an invalid rectangle.
    public RectangleF GetExtent()
    {
      float fTop = 0f, fRight = 0f, fBottom = 0f, fLeft = 0f;
      if( m_alMembers != null)
      {
        bool bFirst = true;
        foreach( CMiniTreeObject obj in m_alMembers )
        {
          if( obj is CMiniTreeIndividual )
          {
            float fIndividualTop = ((CMiniTreeIndividual)obj).Top;
            float fIndividualBottom = ((CMiniTreeIndividual)obj).Bottom;
            float fIndividualLeft = ((CMiniTreeIndividual)obj).Left;
            float fIndividualRight = ((CMiniTreeIndividual)obj).Right;
            if( bFirst || fIndividualTop < fTop )
            {
              fTop = fIndividualTop;
            }
            if( bFirst || fIndividualBottom > fBottom )
            {
              fBottom = fIndividualBottom;
            }
            if( bFirst || fIndividualLeft < fLeft )
            {
              fLeft = fIndividualLeft;
            }
            if( bFirst || fIndividualRight > fRight )
            {
              fRight = fIndividualRight;
            }
            bFirst = false;

          }
          else if( obj is CMiniTreeGroup )
          {
            if( ((CMiniTreeGroup)obj).m_alMembers != null )
            {
              RectangleF rectSubGroup = ((CMiniTreeGroup)obj).GetExtent();

              if( bFirst || rectSubGroup.Top < fTop )
              {
                fTop = rectSubGroup.Top;
              }
              if( bFirst || rectSubGroup.Bottom > fBottom )
              {
                fBottom = rectSubGroup.Bottom;
              }
              if( bFirst || rectSubGroup.Left < fLeft )
              {
                fLeft = rectSubGroup.Left;
              }
              if( bFirst || rectSubGroup.Right > fRight )
              {
                fRight = rectSubGroup.Right;
              }
              bFirst = false;

            }
          }
        }
      }
      return new RectangleF( fLeft, fTop, fRight-fLeft, fBottom-fTop );
    }

    // Moves the position of the boxes in this group and its sub groups by an absolute amount.
    public override void Translate( float fDeltaX, float fDeltaY )
    {
      if( m_alMembers != null)
      {
        foreach( CMiniTreeObject obj in m_alMembers )
        {
          obj.Translate( fDeltaX, fDeltaY );
        }
      }
    }

    // Calculates how to lay out this group to "look right"
    // Must have called CalculateSize on all groups first.
    public override SizeF CalculateLayout( float x, float y )
    {     
      SizeF sizeMax = new SizeF(0f,0f);
      float fStartX = x;
      float fHeight = 0f;
      float fHeightChild = 0f;

      if( m_alMembers == null )
      {
        // Empty group
        return new SizeF( 0f, 0f );
      }

      foreach( object obj in m_alMembers )
      {
        if( obj is CMiniTreeGroup )
        {
          SizeF size = ((CMiniTreeGroup)obj).CalculateLayout(x,y+m_size.Height);
          x += size.Width;
          if( fHeightChild < size.Height )
          {
            fHeightChild = size.Height;
          }       
        }
        else if( obj is CMiniTreeIndividual )
        {
          SizeF size = ((CMiniTreeIndividual)obj).CalculateLayout( x, y );
          x += size.Width;
          if( fHeight < size.Height )
          {
            fHeight = size.Height;
          }
        }
      }

      sizeMax.Width = x - fStartX;
      sizeMax.Height = fHeight + fHeightChild;

      return sizeMax;
    }

    // Improve the layout by moving boxes closer to each other.
    public void Compress()
    {
      if( m_alMembers == null )
      {
        // Empty group
        return;
      }

      foreach( CMiniTreeObject obj in m_alMembers )
      {
        if( obj is CMiniTreeGroup )
        {          
          // Propagate the compression.
          ((CMiniTreeGroup)obj).Compress();
        }
      }

      // Some groups are containers for other groups only (where an individuals 
      // frParents are not known and there is no fr structure for the individual)
      if( m_uStalkedIndividuals>0 )
      {
        SizeF stalkMinMax = StalkMinMax;

        // Width irrelevant, using SizeF simply as a way to pass 2 floats
        float fStalkMin = stalkMinMax.Width; 

        // Height irrelevant, using SizeF simply as a way to pass 2 floats
        float fStalkMax = stalkMinMax.Height; 

        // Pull both halves towards centre
        float fCentre = (fStalkMax + fStalkMin) / 2f; 

        // The following creates 'mooted' coordinates:

        // Pull as much as allowed
        PullLeftStuffRight( fCentre ); 

        // Pull as much as allowed
        PullRightStuffLeft( fCentre ); 
      }
    }

    // Returns the left-most box. (Used when drawing horizontal gedcomLine from which children hang.)
    public CMiniTreeIndividual LeftBox
    {
      get
      {
        return m_mtiBoxLeft;
      }
      set
      {
        m_mtiBoxLeft = value;
      }
    }

    // Returns the right-most box. (Used when drawing horizontal gedcomLine from which children hang.)
    public CMiniTreeIndividual RightBox
    {
      get
      {
        return m_mtiBoxRight;
      }
      set
      {
        m_mtiBoxRight = value;
      }
    }

    // Shifts this object and all objects to its left, until one can't move.
    public override float PullLeft(float fAmount)
    {
      // Did this couple have children?
      if( m_alMembers != null )
      {
        // Shift the underhanging group members
        // Find the rightmost underhanging member and shift it left. 
        // That will interact with other members to find max possible shift amount.
        CMiniTreeIndividual mtiRightmost = null;
        float fMax = 0;
        bool bFirst = true;
        foreach( CMiniTreeObject obj in m_alMembers )
        {
          if( obj is CMiniTreeIndividual )
          {
            if( bFirst || ((CMiniTreeIndividual)obj).Left > fMax )
            {
              fMax = ((CMiniTreeIndividual)obj).Left;
              mtiRightmost = ((CMiniTreeIndividual)obj);
            }
            bFirst = false;
          }
        }
        if( mtiRightmost != null )
        {
          fAmount = mtiRightmost.PushLeft( fAmount );
        }
      }

      // Now shift right object left.
      CMiniTreeObject mtoRight = RightObject;
      if( mtoRight != null )
      {
        fAmount = mtoRight.PullLeft( fAmount );
      }

      return fAmount;
    }

    // Shifts this object and all objects to its right, until one can't move.
    public override float PullRight( float fAmount )
    {
      if( m_alMembers != null )
      {
        // This couple had children

        // Shift the underhanging group members.
        // Find the leftmost underhanging member and shift it right. 
        // That will interact with other members to find max possible shift amount.
        CMiniTreeIndividual mtiLeftmost = null;
        float fMin = 0;
        bool fFirst = true;
        foreach( CMiniTreeObject obj in m_alMembers )
        {
          if( obj is CMiniTreeIndividual )
          {
            if( fFirst || ((CMiniTreeIndividual)obj).Right < fMin )
            {
              fMin = ((CMiniTreeIndividual)obj).Right;
              mtiLeftmost = ((CMiniTreeIndividual)obj);
            }
            fFirst = false;
          }
        }
        if( mtiLeftmost != null )
        {
          fAmount = mtiLeftmost.PushRight( fAmount );
        }
      }
      
      // Now shift left object right.
      CMiniTreeObject mtoLeft = LeftObject;
      if( mtoLeft != null )
      {
        fAmount = mtoLeft.PullRight( fAmount );
      }

      return fAmount;
    }

    // Pushes this object left and all objects to its left left, until one can't move.
    public override float PushLeft( float fAmount )
    {
      // TODO: Not yet implemented - compression won't be optimal.
      return fAmount;
    }

    // Pushes this object right and all objects to its right right, until one can't move.
    public override float PushRight( float fAmount )
    {
      // TODO: Not yet implemented - compression won't be optimal.
      return fAmount;
    }


    // Returns minimum and maximum x coordinate for an upwards gedcomLine from this group's crossbar.
    // Using SizeF as a way to pass 2 floats.
    // Caller should check m_nIndividuals first to make sure this property is valid.
    private SizeF StalkMinMax
    {
      get
      {
        float fMin = 0f;
        float fMax = 0f;
        bool bFirst = true;
        foreach( CMiniTreeObject obj in m_alMembers )
        {
          if( obj is CMiniTreeIndividual )
          {
            if( ((CMiniTreeIndividual)obj).HasStalk )
            {
              float fStalk = ((CMiniTreeIndividual)obj).Stalk;
              if( bFirst || fStalk < fMin )
              {
                fMin = fStalk;
              }
              if( bFirst || fStalk > fMax )
              {
                fMax = fStalk;
              }
              bFirst = false;
            }
          }
        }

        return new SizeF( fMin, fMax );
      }
    }

    // Pulls left box as close as can be, to compress group.
    // Gets left box to pull its left box/group in turn.
    // Boxes are free to move. Moving a group will pull its members.
    // They must not collide with other boxes.
    private void PullLeftStuffRight( float fCentre )
    {
      CMiniTreeObject mtoLeft = LeftObject;
      if( mtoLeft != null )
      {
        if( mtoLeft is CMiniTreeIndividual )
        {
          mtoLeft.PullRight(fCentre - ((CMiniTreeIndividual)mtoLeft).Right);
        }
      }
    }

    // Pulls right box as close as can be, to compress group.
    private void PullRightStuffLeft( float fCentre )
    {
      CMiniTreeObject mtoRight = RightObject;
      if( mtoRight != null )
      {
        if( mtoRight is CMiniTreeIndividual )
        {
          mtoRight.PullLeft(((CMiniTreeIndividual)mtoRight).Left - fCentre);
        }
      }
    }
  }
}
