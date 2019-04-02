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

using System.Collections.Generic;
using System.Drawing;
using GKCommon.GEDCOM;

namespace GEDmill.MiniTree
{
    /// <summary>
    /// Represents a group of individuals in the tree, e.g. a group of siblings, that need to be kept together
    /// </summary>
    public class MiniTreeGroup : MiniTreeObject
    {
        // Type of gedcomLine joining this box horizontally to indicate marriage or other parenting relationship.
        public enum ECrossbar
        {
            Solid = 0,
            DottedLeft = 1,
            DottedRight = 2
        }

        // The CMiniTreeIndividual boxes and groups that make up this group.
        private List<MiniTreeObject> fMembers;

        // Width of the gedcomLine joining this group to group above.
        private const float TEE_WIDTH = 0.0f;

        // Height of the gedcomLine joining this group to group above.
        private const float TEE_HEIGHT = 16.0f;

        // The screen size of this group
        private SizeF fSize;

        // The group that this group belongs to.
        private MiniTreeGroup fParent;

        // The number of individuals in this group (as opposed to groups)
        private uint fIndividuals;

        // Number of individuals in this group with lines up to the generation above.
        private uint fStalkedIndividuals;

        // The left-most box.
        private MiniTreeIndividual fBoxLeft;

        // The right-most box.
        private MiniTreeIndividual fBoxRight;

        // Reference to the last box to be added to this group
        private MiniTreeObject fLastAddedObject;

        // Type of gedcomLine joining this box horizontally to indicate marriage or other parenting relationship.
        public ECrossbar fCrossbar;


        // Returns the left-most box. (Used when drawing horizontal gedcomLine from which children hang.)
        public MiniTreeIndividual LeftBox
        {
            get { return fBoxLeft; }
            set { fBoxLeft = value; }
        }

        // Returns the right-most box. (Used when drawing horizontal gedcomLine from which children hang.)
        public MiniTreeIndividual RightBox
        {
            get { return fBoxRight; }
            set { fBoxRight = value; }
        }


        public MiniTreeGroup()
        {
            fMembers = null;
            fSize = new SizeF(0.0f, 0.0f);
            fParent = null;
            fIndividuals = 0;
            fStalkedIndividuals = 0;
            fBoxLeft = null;
            fBoxRight = null;
            fLastAddedObject = null;
            fCrossbar = ECrossbar.Solid;
        }

        // Creates a CMiniTreeIndividual for the individual specified and adds it to the group.
        // Informs neighbouring boxes about this box.
        // bCreateLink decides whether to make this box a clickable link in the HTML.
        public MiniTreeIndividual AddIndividual(GEDCOMIndividualRecord ir, string firstnames, string surname,
                                                 string date, bool createLink, bool createStalk, bool highlight,
                                                 bool concealed, bool shade)
        {
            MiniTreeIndividual mti = new MiniTreeIndividual(ir, firstnames, surname, date, createLink, createStalk,
                                                              highlight, concealed, shade, CConfig.Instance.ConserveTreeWidth);

            if (fMembers == null) {
                fMembers = new List<MiniTreeObject>();
            }
            fMembers.Add(mti);

            fIndividuals++;

            if (createStalk) {
                fStalkedIndividuals++;
            }

            mti.LeftObject = fLastAddedObject;

            if (fLastAddedObject != null) {
                fLastAddedObject.RightObject = mti;
            }

            fLastAddedObject = mti;

            return mti;
        }

        // Adds a  CMiniTreeGroup to this group.
        // Informs neighbouring boxes about the group.
        public void AddGroup(MiniTreeGroup mtg)
        {
            if (mtg != null) {
                if (fMembers == null) {
                    fMembers = new List<MiniTreeObject>();
                }
                fMembers.Add(mtg);

                mtg.fParent = this;

                mtg.LeftObject = fLastAddedObject;

                if (fLastAddedObject != null) {
                    fLastAddedObject.RightObject = mtg;
                }

                fLastAddedObject = mtg;
            }
        }

        // Calculates the size required by this group. Initialises the class fields
        // that contain size information. Returns the overall group size.
        public override SizeF CalculateSize(Graphics g, Font f)
        {
            fSize.Width = 0.0f;
            fSize.Height = 0.0f;

            if (fMembers == null) {
                // Empty group
                return fSize;
            }

            foreach (object obj in fMembers) {
                SizeF size;
                if (obj is MiniTreeIndividual) {
                    size = ((MiniTreeIndividual)obj).CalculateSize(g, f);
                } else if (obj is MiniTreeGroup) {
                    // Let group calculate its size for later
                    ((MiniTreeGroup)obj).CalculateSize(g, f);

                    // Size here is only size of tee
                    size = new SizeF(TEE_WIDTH, TEE_HEIGHT);
                } else {
                    size = new SizeF(0f, 0f);
                }

                fSize.Width += size.Width;
                if (size.Height > fSize.Height) {
                    fSize.Height = size.Height;
                }
            }

            if (fIndividuals == 0) {
                // Don't include tee size if no individuals
                fSize.Width = 0f;
                fSize.Height = 0f;
            }

            return fSize;
        }

        // Draws the group to the graphics instance.
        public override void DrawBitmap(Paintbox paintbox, Graphics g, List<MiniTreeMap> alMap)
        {
            if (fMembers == null) {
                // Empty group
                return;
            }

            foreach (object obj in fMembers) {
                if (obj is MiniTreeGroup) {
                    if (((MiniTreeGroup)obj).fBoxLeft != null && ((MiniTreeGroup)obj).fBoxRight != null) {
                        // Draw crossbar
                        float fCrossbarLeft = ((MiniTreeGroup)obj).fBoxLeft.TeeRight;
                        float fCrossbarRight = ((MiniTreeGroup)obj).fBoxRight.TeeLeft;
                        float fCrossbarLeftGap = ((MiniTreeGroup)obj).fBoxLeft.Right;
                        float fCrossbarRightGap = ((MiniTreeGroup)obj).fBoxRight.Left;
                        float fCrossbarY = (((MiniTreeGroup)obj).fBoxLeft.TeeCentreVert + ((MiniTreeGroup)obj).fBoxRight.TeeCentreVert) / 2f;
                        switch (((MiniTreeGroup)obj).fCrossbar) {
                            case ECrossbar.Solid:
                                g.DrawLine(paintbox.PenConnector, fCrossbarLeft, fCrossbarY, fCrossbarRight, fCrossbarY);
                                break;

                            case ECrossbar.DottedLeft:
                                g.DrawLine(paintbox.PenConnectorDotted, fCrossbarLeft, fCrossbarY, fCrossbarRightGap, fCrossbarY);
                                break;

                            case ECrossbar.DottedRight:
                                g.DrawLine(paintbox.PenConnectorDotted, fCrossbarLeftGap, fCrossbarY, fCrossbarRight, fCrossbarY);
                                break;

                            default:
                                break;
                        }

                        if (((MiniTreeGroup)obj).fStalkedIndividuals > 0) {
                            // Draw down to individuals
                            // Use y coord of first individual, assuming all are at the same y coord
                            float fIndividualY = 0f;
                            bool bHaveIndividuals = false;
                            foreach (MiniTreeObject groupObj in ((MiniTreeGroup)obj).fMembers) {
                                if (groupObj is MiniTreeIndividual) {
                                    fIndividualY = ((MiniTreeIndividual)groupObj).Top;
                                    bHaveIndividuals = true;
                                    break;
                                }
                            }
                            float fCrossbarCentre = (fCrossbarLeft + fCrossbarRight) / 2f;
                            if (bHaveIndividuals) {
                                g.DrawLine(paintbox.PenConnector, fCrossbarCentre, fCrossbarY, fCrossbarCentre, fIndividualY);

                                // Connect individuals
                                SizeF stalkMinMax = ((MiniTreeGroup)obj).StalkMinMax;

                                // Width irrelevant, using SizeF simply as a way to pass 2 floats:
                                float fStalkMin = stalkMinMax.Width;

                                // Height irrelevant, using SizeF simply as a way to pass 2 floats
                                float fStalkMax = stalkMinMax.Height;

                                if (fCrossbarCentre < fStalkMin) {
                                    fStalkMin = fCrossbarCentre;
                                } else if (fCrossbarCentre > fStalkMax) {
                                    fStalkMax = fCrossbarCentre;
                                }
                                g.DrawLine(paintbox.PenConnector, fStalkMin, fIndividualY, fStalkMax, fIndividualY);
                            }
                        }
                    }

                    ((MiniTreeGroup)obj).DrawBitmap(paintbox, g, alMap);
                } else if (obj is MiniTreeIndividual) {
                    // Draw individual box
                    ((MiniTreeIndividual)obj).DrawBitmap(paintbox, g, alMap);
                }
            }
        }

        // Returns the size occupied by all the boxes in this group and its sub groups.
        // Caller must ensure members != null otherwise they will get returned an invalid rectangle.
        public RectangleF GetExtent()
        {
            float fTop = 0f, fRight = 0f, fBottom = 0f, fLeft = 0f;
            if (fMembers != null) {
                bool bFirst = true;
                foreach (MiniTreeObject obj in fMembers) {
                    if (obj is MiniTreeIndividual) {
                        float fIndividualTop = ((MiniTreeIndividual)obj).Top;
                        float fIndividualBottom = ((MiniTreeIndividual)obj).Bottom;
                        float fIndividualLeft = ((MiniTreeIndividual)obj).Left;
                        float fIndividualRight = ((MiniTreeIndividual)obj).Right;
                        if (bFirst || fIndividualTop < fTop) {
                            fTop = fIndividualTop;
                        }
                        if (bFirst || fIndividualBottom > fBottom) {
                            fBottom = fIndividualBottom;
                        }
                        if (bFirst || fIndividualLeft < fLeft) {
                            fLeft = fIndividualLeft;
                        }
                        if (bFirst || fIndividualRight > fRight) {
                            fRight = fIndividualRight;
                        }
                        bFirst = false;
                    } else if (obj is MiniTreeGroup) {
                        if (((MiniTreeGroup)obj).fMembers != null) {
                            RectangleF rectSubGroup = ((MiniTreeGroup)obj).GetExtent();

                            if (bFirst || rectSubGroup.Top < fTop) {
                                fTop = rectSubGroup.Top;
                            }
                            if (bFirst || rectSubGroup.Bottom > fBottom) {
                                fBottom = rectSubGroup.Bottom;
                            }
                            if (bFirst || rectSubGroup.Left < fLeft) {
                                fLeft = rectSubGroup.Left;
                            }
                            if (bFirst || rectSubGroup.Right > fRight) {
                                fRight = rectSubGroup.Right;
                            }
                            bFirst = false;
                        }
                    }
                }
            }
            return new RectangleF(fLeft, fTop, fRight - fLeft, fBottom - fTop);
        }

        // Moves the position of the boxes in this group and its sub groups by an absolute amount.
        public override void Translate(float fDeltaX, float fDeltaY)
        {
            if (fMembers != null) {
                foreach (MiniTreeObject obj in fMembers) {
                    obj.Translate(fDeltaX, fDeltaY);
                }
            }
        }

        // Calculates how to lay out this group to "look right"
        // Must have called CalculateSize on all groups first.
        public override SizeF CalculateLayout(float x, float y)
        {
            SizeF sizeMax = new SizeF(0f, 0f);
            float fStartX = x;
            float fHeight = 0f;
            float fHeightChild = 0f;

            if (fMembers == null) {
                // Empty group
                return new SizeF(0f, 0f);
            }

            foreach (object obj in fMembers) {
                if (obj is MiniTreeGroup) {
                    SizeF size = ((MiniTreeGroup)obj).CalculateLayout(x, y + fSize.Height);
                    x += size.Width;
                    if (fHeightChild < size.Height) {
                        fHeightChild = size.Height;
                    }
                } else if (obj is MiniTreeIndividual) {
                    SizeF size = ((MiniTreeIndividual)obj).CalculateLayout(x, y);
                    x += size.Width;
                    if (fHeight < size.Height) {
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
            if (fMembers == null) {
                // Empty group
                return;
            }

            foreach (MiniTreeObject obj in fMembers) {
                var mtGroup = obj as MiniTreeGroup;
                if (mtGroup != null) {
                    // Propagate the compression.
                    mtGroup.Compress();
                }
            }

            // Some groups are containers for other groups only (where an individuals 
            // frParents are not known and there is no fr structure for the individual)
            if (fStalkedIndividuals > 0) {
                SizeF stalkMinMax = StalkMinMax;

                // Width irrelevant, using SizeF simply as a way to pass 2 floats
                float fStalkMin = stalkMinMax.Width;

                // Height irrelevant, using SizeF simply as a way to pass 2 floats
                float fStalkMax = stalkMinMax.Height;

                // Pull both halves towards centre
                float fCentre = (fStalkMax + fStalkMin) / 2f;

                // The following creates 'mooted' coordinates:

                // Pull as much as allowed
                PullLeftStuffRight(fCentre);

                // Pull as much as allowed
                PullRightStuffLeft(fCentre);
            }
        }

        // Shifts this object and all objects to its left, until one can't move.
        public override float PullLeft(float fAmount)
        {
            // Did this couple have children?
            if (fMembers != null) {
                // Shift the underhanging group members
                // Find the rightmost underhanging member and shift it left. 
                // That will interact with other members to find max possible shift amount.
                MiniTreeIndividual mtiRightmost = null;
                float fMax = 0;
                bool bFirst = true;
                foreach (MiniTreeObject obj in fMembers) {
                    var mtIndi = obj as MiniTreeIndividual;
                    if (mtIndi != null) {
                        if (bFirst || mtIndi.Left > fMax) {
                            fMax = mtIndi.Left;
                            mtiRightmost = mtIndi;
                        }
                        bFirst = false;
                    }
                }
                if (mtiRightmost != null) {
                    fAmount = mtiRightmost.PushLeft(fAmount);
                }
            }

            // Now shift right object left.
            MiniTreeObject mtoRight = RightObject;
            if (mtoRight != null) {
                fAmount = mtoRight.PullLeft(fAmount);
            }

            return fAmount;
        }

        // Shifts this object and all objects to its right, until one can't move.
        public override float PullRight(float fAmount)
        {
            if (fMembers != null) {
                // This couple had children

                // Shift the underhanging group members.
                // Find the leftmost underhanging member and shift it right. 
                // That will interact with other members to find max possible shift amount.
                MiniTreeIndividual mtiLeftmost = null;
                float fMin = 0;
                bool fFirst = true;
                foreach (MiniTreeObject obj in fMembers) {
                    var mtIndi = obj as MiniTreeIndividual;
                    if (mtIndi != null) {
                        if (fFirst || mtIndi.Right < fMin) {
                            fMin = mtIndi.Right;
                            mtiLeftmost = mtIndi;
                        }
                        fFirst = false;
                    }
                }
                if (mtiLeftmost != null) {
                    fAmount = mtiLeftmost.PushRight(fAmount);
                }
            }

            // Now shift left object right.
            MiniTreeObject mtoLeft = LeftObject;
            if (mtoLeft != null) {
                fAmount = mtoLeft.PullRight(fAmount);
            }

            return fAmount;
        }

        // Pushes this object left and all objects to its left left, until one can't move.
        public override float PushLeft(float fAmount)
        {
            // TODO: Not yet implemented - compression won't be optimal.
            return fAmount;
        }

        // Pushes this object right and all objects to its right right, until one can't move.
        public override float PushRight(float fAmount)
        {
            // TODO: Not yet implemented - compression won't be optimal.
            return fAmount;
        }


        // Returns minimum and maximum x coordinate for an upwards gedcomLine from this group's crossbar.
        // Using SizeF as a way to pass 2 floats.
        // Caller should check m_nIndividuals first to make sure this property is valid.
        private SizeF StalkMinMax
        {
            get {
                float fMin = 0f;
                float fMax = 0f;
                bool bFirst = true;
                foreach (MiniTreeObject obj in fMembers) {
                    var mtIndi = obj as MiniTreeIndividual;
                    if (mtIndi != null && mtIndi.HasStalk) {
                        float fStalk = mtIndi.Stalk;
                        if (bFirst || fStalk < fMin) {
                            fMin = fStalk;
                        }
                        if (bFirst || fStalk > fMax) {
                            fMax = fStalk;
                        }
                        bFirst = false;
                    }
                }

                return new SizeF(fMin, fMax);
            }
        }

        // Pulls left box as close as can be, to compress group.
        // Gets left box to pull its left box/group in turn.
        // Boxes are free to move. Moving a group will pull its members.
        // They must not collide with other boxes.
        private void PullLeftStuffRight(float fCentre)
        {
            var mtoLeft = LeftObject as MiniTreeIndividual;
            if (mtoLeft != null) {
                mtoLeft.PullRight(fCentre - mtoLeft.Right);
            }
        }

        // Pulls right box as close as can be, to compress group.
        private void PullRightStuffLeft(float fCentre)
        {
            var mtoRight = RightObject as MiniTreeIndividual;
            if (mtoRight != null) {
                mtoRight.PullLeft(mtoRight.Left - fCentre);
            }
        }
    }
}
