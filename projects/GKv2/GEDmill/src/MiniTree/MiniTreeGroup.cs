/* 
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
 */

using System.Collections.Generic;
using System.Drawing;
using GDModel;

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
        public MiniTreeIndividual AddIndividual(GDMIndividualRecord ir, string firstnames, string surname,
                                                string date, bool createLink, bool createStalk, bool highlight,
                                                bool concealed, bool shade)
        {
            MiniTreeIndividual mti = new MiniTreeIndividual(ir, firstnames, surname, date, createLink, createStalk,
                                                            highlight, concealed, shade, GMConfig.Instance.ConserveTreeWidth);

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

            foreach (var obj in fMembers) {
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
        public override void DrawBitmap(Paintbox paintbox, Graphics g, List<MiniTreeMap> map)
        {
            if (fMembers == null) {
                // Empty group
                return;
            }

            foreach (var obj in fMembers) {
                if (obj is MiniTreeGroup) {
                    var mtg = (MiniTreeGroup)obj;
                    if (mtg.fBoxLeft != null && mtg.fBoxRight != null) {
                        // Draw crossbar
                        float crossbarLeft = mtg.fBoxLeft.TeeRight;
                        float crossbarRight = mtg.fBoxRight.TeeLeft;
                        float crossbarLeftGap = mtg.fBoxLeft.Right;
                        float crossbarRightGap = mtg.fBoxRight.Left;
                        float crossbarY = (mtg.fBoxLeft.TeeCentreVert + mtg.fBoxRight.TeeCentreVert) / 2f;
                        switch (mtg.fCrossbar) {
                            case ECrossbar.Solid:
                                g.DrawLine(paintbox.PenConnector, crossbarLeft, crossbarY, crossbarRight, crossbarY);
                                break;

                            case ECrossbar.DottedLeft:
                                g.DrawLine(paintbox.PenConnectorDotted, crossbarLeft, crossbarY, crossbarRightGap, crossbarY);
                                break;

                            case ECrossbar.DottedRight:
                                g.DrawLine(paintbox.PenConnectorDotted, crossbarLeftGap, crossbarY, crossbarRight, crossbarY);
                                break;
                        }

                        if (mtg.fStalkedIndividuals > 0) {
                            // Draw down to individuals
                            // Use y coord of first individual, assuming all are at the same y coord
                            float individualY = 0f;
                            bool haveIndividuals = false;
                            foreach (MiniTreeObject groupObj in mtg.fMembers) {
                                if (groupObj is MiniTreeIndividual) {
                                    individualY = ((MiniTreeIndividual)groupObj).Top;
                                    haveIndividuals = true;
                                    break;
                                }
                            }
                            float crossbarCentre = (crossbarLeft + crossbarRight) / 2f;
                            if (haveIndividuals) {
                                g.DrawLine(paintbox.PenConnector, crossbarCentre, crossbarY, crossbarCentre, individualY);

                                // Connect individuals
                                SizeF stalkMinMax = mtg.StalkMinMax;

                                // Width irrelevant, using SizeF simply as a way to pass 2 floats:
                                float stalkMin = stalkMinMax.Width;

                                // Height irrelevant, using SizeF simply as a way to pass 2 floats
                                float stalkMax = stalkMinMax.Height;

                                if (crossbarCentre < stalkMin) {
                                    stalkMin = crossbarCentre;
                                } else if (crossbarCentre > stalkMax) {
                                    stalkMax = crossbarCentre;
                                }
                                g.DrawLine(paintbox.PenConnector, stalkMin, individualY, stalkMax, individualY);
                            }
                        }
                    }

                    mtg.DrawBitmap(paintbox, g, map);
                } else if (obj is MiniTreeIndividual) {
                    // Draw individual box
                    ((MiniTreeIndividual)obj).DrawBitmap(paintbox, g, map);
                }
            }
        }

        // Returns the size occupied by all the boxes in this group and its sub groups.
        // Caller must ensure members != null otherwise they will get returned an invalid rectangle.
        public RectangleF GetExtent()
        {
            float top = 0f, right = 0f, bottom = 0f, left = 0f;
            if (fMembers != null) {
                bool first = true;
                foreach (MiniTreeObject obj in fMembers) {
                    if (obj is MiniTreeIndividual) {
                        var mtIndi = (MiniTreeIndividual)obj;
                        float individualTop = mtIndi.Top;
                        float individualBottom = mtIndi.Bottom;
                        float individualLeft = mtIndi.Left;
                        float individualRight = mtIndi.Right;
                        if (first || individualTop < top) {
                            top = individualTop;
                        }
                        if (first || individualBottom > bottom) {
                            bottom = individualBottom;
                        }
                        if (first || individualLeft < left) {
                            left = individualLeft;
                        }
                        if (first || individualRight > right) {
                            right = individualRight;
                        }
                        first = false;
                    } else if (obj is MiniTreeGroup) {
                        if (((MiniTreeGroup)obj).fMembers != null) {
                            RectangleF rectSubGroup = ((MiniTreeGroup)obj).GetExtent();

                            if (first || rectSubGroup.Top < top) {
                                top = rectSubGroup.Top;
                            }
                            if (first || rectSubGroup.Bottom > bottom) {
                                bottom = rectSubGroup.Bottom;
                            }
                            if (first || rectSubGroup.Left < left) {
                                left = rectSubGroup.Left;
                            }
                            if (first || rectSubGroup.Right > right) {
                                right = rectSubGroup.Right;
                            }
                            first = false;
                        }
                    }
                }
            }
            return new RectangleF(left, top, right - left, bottom - top);
        }

        // Moves the position of the boxes in this group and its sub groups by an absolute amount.
        public override void Translate(float deltaX, float deltaY)
        {
            if (fMembers != null) {
                foreach (MiniTreeObject obj in fMembers) {
                    obj.Translate(deltaX, deltaY);
                }
            }
        }

        // Calculates how to lay out this group to "look right"
        // Must have called CalculateSize on all groups first.
        public override SizeF CalculateLayout(float x, float y)
        {
            SizeF sizeMax = new SizeF(0f, 0f);
            float startX = x;
            float height = 0f;
            float heightChild = 0f;

            if (fMembers == null) {
                // Empty group
                return new SizeF(0f, 0f);
            }

            foreach (var obj in fMembers) {
                if (obj is MiniTreeGroup) {
                    SizeF size = ((MiniTreeGroup)obj).CalculateLayout(x, y + fSize.Height);
                    x += size.Width;
                    if (heightChild < size.Height) {
                        heightChild = size.Height;
                    }
                } else if (obj is MiniTreeIndividual) {
                    SizeF size = ((MiniTreeIndividual)obj).CalculateLayout(x, y);
                    x += size.Width;
                    if (height < size.Height) {
                        height = size.Height;
                    }
                }
            }

            sizeMax.Width = x - startX;
            sizeMax.Height = height + heightChild;
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
                float stalkMin = stalkMinMax.Width;

                // Height irrelevant, using SizeF simply as a way to pass 2 floats
                float stalkMax = stalkMinMax.Height;

                // Pull both halves towards centre
                float centre = (stalkMax + stalkMin) / 2f;

                // The following creates 'mooted' coordinates:

                // Pull as much as allowed
                PullLeftStuffRight(centre);

                // Pull as much as allowed
                PullRightStuffLeft(centre);
            }
        }

        // Shifts this object and all objects to its left, until one can't move.
        public override float PullLeft(float amount)
        {
            // Did this couple have children?
            if (fMembers != null) {
                // Shift the underhanging group members
                // Find the rightmost underhanging member and shift it left. 
                // That will interact with other members to find max possible shift amount.
                MiniTreeIndividual mtiRightmost = null;
                float max = 0;
                bool first = true;
                foreach (MiniTreeObject obj in fMembers) {
                    var mtIndi = obj as MiniTreeIndividual;
                    if (mtIndi != null) {
                        if (first || mtIndi.Left > max) {
                            max = mtIndi.Left;
                            mtiRightmost = mtIndi;
                        }
                        first = false;
                    }
                }
                if (mtiRightmost != null) {
                    amount = mtiRightmost.PushLeft(amount);
                }
            }

            // Now shift right object left.
            MiniTreeObject mtoRight = RightObject;
            if (mtoRight != null) {
                amount = mtoRight.PullLeft(amount);
            }

            return amount;
        }

        // Shifts this object and all objects to its right, until one can't move.
        public override float PullRight(float amount)
        {
            if (fMembers != null) {
                // This couple had children

                // Shift the underhanging group members.
                // Find the leftmost underhanging member and shift it right. 
                // That will interact with other members to find max possible shift amount.
                MiniTreeIndividual mtiLeftmost = null;
                float min = 0;
                bool first = true;
                foreach (MiniTreeObject obj in fMembers) {
                    var mtIndi = obj as MiniTreeIndividual;
                    if (mtIndi != null) {
                        if (first || mtIndi.Right < min) {
                            min = mtIndi.Right;
                            mtiLeftmost = mtIndi;
                        }
                        first = false;
                    }
                }
                if (mtiLeftmost != null) {
                    amount = mtiLeftmost.PushRight(amount);
                }
            }

            // Now shift left object right.
            MiniTreeObject mtoLeft = LeftObject;
            if (mtoLeft != null) {
                amount = mtoLeft.PullRight(amount);
            }

            return amount;
        }

        // Pushes this object left and all objects to its left left, until one can't move.
        public override float PushLeft(float amount)
        {
            // TODO: Not yet implemented - compression won't be optimal.
            return amount;
        }

        // Pushes this object right and all objects to its right right, until one can't move.
        public override float PushRight(float amount)
        {
            // TODO: Not yet implemented - compression won't be optimal.
            return amount;
        }


        // Returns minimum and maximum x coordinate for an upwards gedcomLine from this group's crossbar.
        // Using SizeF as a way to pass 2 floats.
        // Caller should check m_nIndividuals first to make sure this property is valid.
        private SizeF StalkMinMax
        {
            get {
                float min = 0f;
                float max = 0f;
                bool first = true;
                foreach (MiniTreeObject obj in fMembers) {
                    var mtIndi = obj as MiniTreeIndividual;
                    if (mtIndi != null && mtIndi.HasStalk) {
                        float fStalk = mtIndi.Stalk;
                        if (first || fStalk < min) {
                            min = fStalk;
                        }
                        if (first || fStalk > max) {
                            max = fStalk;
                        }
                        first = false;
                    }
                }

                return new SizeF(min, max);
            }
        }

        // Pulls left box as close as can be, to compress group.
        // Gets left box to pull its left box/group in turn.
        // Boxes are free to move. Moving a group will pull its members.
        // They must not collide with other boxes.
        private void PullLeftStuffRight(float centre)
        {
            var mtoLeft = LeftObject as MiniTreeIndividual;
            if (mtoLeft != null) {
                mtoLeft.PullRight(centre - mtoLeft.Right);
            }
        }

        // Pulls right box as close as can be, to compress group.
        private void PullRightStuffLeft(float centre)
        {
            var mtoRight = RightObject as MiniTreeIndividual;
            if (mtoRight != null) {
                mtoRight.PullLeft(mtoRight.Left - centre);
            }
        }
    }
}
