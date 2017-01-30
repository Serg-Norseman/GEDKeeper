/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKUI.Charts
{
    public class AncestorsCircle : CircleChart
    {
        private class PersonSegment : CircleSegment
        {
            public int GroupIndex;
            public PersonSegment FatherSegment;
            public PersonSegment MotherSegment;

            public PersonSegment(int generation) : base(generation)
            {
                GroupIndex = -1;
                FatherSegment = null;
                MotherSegment = null;
            }
        }


        private int fGroupCount;
        private bool fGroupsMode;


        public bool GroupsMode
        {
            get {
                return fGroupsMode;
            }
            set {
                fGroupsMode = value;
                if (value) {
                    fMaxGenerations = 8;
                    GenWidth = 60;
                } else {
                    fMaxGenerations = 8;
                    GenWidth = DEFAULT_GEN_WIDTH;
                }
            }
        }


        public AncestorsCircle(IBaseWindow baseWin) : base(baseWin)
        {
        }

        protected override void BuildPathTree()
        {
            fSegments.Clear();

            int inRad = CENTER_RAD - 50;

            PersonSegment segment = new PersonSegment(0);
            GraphicsPath path = segment.Path;
            path.StartFigure();
            path.AddEllipse(-inRad, -inRad, inRad << 1, inRad << 1);
            path.CloseFigure();
            fSegments.Add(segment);

            int maxSteps = 1;
            for (int gen = 1; gen <= fMaxGenerations; gen++) {
                inRad = (CENTER_RAD - 50) + ((gen - 1) * fGenWidth);

                int extRad = inRad + fGenWidth;
                maxSteps *= 2;

                float stepAngle = (360.0f / maxSteps);

                for (int stp = 0; stp < maxSteps; stp++)
                {
                    float ang1 = (stp * stepAngle) - 90.0f;
                    float ang2 = ang1 + stepAngle;

                    segment = new PersonSegment(gen);
                    segment.StartAngle = ang1;
                    segment.WedgeAngle = stepAngle;
                    CreateCircleSegment(segment.Path, inRad, extRad, stepAngle, ang1, ang2);
                    fSegments.Add(segment);
                }
            }

            // traverse tree
            fGroupCount = -1;
            fIndividualsCount = 0;
            if (fRootPerson != null) {
                fIndividualsCount++;
                PersonSegment rootSegment = SetSegmentParams(0, fRootPerson, 0, -1);
                rootSegment.WedgeAngle = 360.0f;

                GEDCOMIndividualRecord father, mother;
                fRootPerson.GetParents(out father, out mother);

                if (mother != null) {
                    rootSegment.MotherSegment = TraverseAncestors(mother, 90f, 1, CENTER_RAD, 90.0f, 1, -1);
                }

                if (father != null) {
                    rootSegment.FatherSegment = TraverseAncestors(father, 270.0f, 1, CENTER_RAD, 90.0f, 1, -1);
                }
            }
        }

        private PersonSegment SetSegmentParams(int index, GEDCOMIndividualRecord rec, int rad, int groupIndex)
        {
            if (index < 0 || index >= fSegments.Count) {
                return null;
            } else {
                PersonSegment segment = (PersonSegment)fSegments[index];
                segment.IRec = rec;
                segment.Rad = rad;
                segment.GroupIndex = groupIndex;
                return segment;
            }
        }

        private PersonSegment TraverseAncestors(GEDCOMIndividualRecord iRec, float v, int gen, int rad, float ro, int prevSteps, int groupIndex)
        {
            try
            {
                fIndividualsCount++;

                if (fGroupsMode && groupIndex == -1) {
                    PersonSegment otherSegment = (PersonSegment)FindSegmentByRec(iRec);
                    if (otherSegment != null) {
                        fGroupCount++;
                        groupIndex = fGroupCount;
                        TraverseGroups(otherSegment, groupIndex);
                    }
                }

                int genSize = (int)Math.Pow(2.0, gen);
                float ang = (360.0f / genSize);

                int idx = prevSteps + (int)(v / ang);
                PersonSegment segment = SetSegmentParams(idx, iRec, rad, groupIndex);

                if (segment != null && gen < fMaxGenerations)
                {
                    GEDCOMIndividualRecord father, mother;
                    iRec.GetParents(out father, out mother);

                    int ps = prevSteps + genSize;

                    if (father != null) {
                        v -= (Math.Abs(ang - ro) / 2.0f);
                        segment.FatherSegment = TraverseAncestors(father, v, gen + 1, rad + fGenWidth, ro / 2.0f, ps, groupIndex);
                    }

                    if (mother != null) {
                        v += (ang / 2.0f);
                        segment.MotherSegment = TraverseAncestors(mother, v, gen + 1, rad + fGenWidth, ro / 2.0f, ps, groupIndex);
                    }
                }

                return segment;
            }
            catch
            {
                return null;
            }
        }

        private void TraverseGroups(PersonSegment segment, int groupIndex)
        {
            if (segment == null) return;

            segment.GroupIndex = groupIndex;
            if (segment.FatherSegment != null) TraverseGroups(segment.FatherSegment, groupIndex);
            if (segment.MotherSegment != null) TraverseGroups(segment.MotherSegment, groupIndex);
        }

        protected override void InternalDraw(Graphics gfx)
        {
            gfx.SmoothingMode = SmoothingMode.AntiAlias;

            int numberOfSegments = fSegments.Count;
            for (int i = 0; numberOfSegments > i; ++i) {
                PersonSegment segment = (PersonSegment)fSegments[i];

                bool draw = (!Options.HideEmptySegments || segment.IRec != null);

                if (draw) {
                    int brIndex;
                    if (fGroupsMode) {
                        brIndex = (segment.GroupIndex == -1) ? 11 : segment.GroupIndex;
                    } else {
                        brIndex = (segment.Gen == 0) ? 9 : segment.Gen - 1;
                    }

                    SolidBrush brush = (fSelected == segment) ? fDarkBrushes[brIndex] : fCircleBrushes[brIndex];

                    GraphicsPath path = segment.Path;
                    gfx.FillPath(brush, path);
                    gfx.DrawPath(fPen, path);
                }

                DrawPersonName(gfx, fSegments[i]);
            }
        }

        protected override void OnDoubleClick(EventArgs e)
        {
            base.OnDoubleClick(e);
            GroupsMode = !GroupsMode;
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            base.OnKeyDown(e);

            e.Handled = false;
            switch (e.KeyCode) {
                case Keys.Add:
                    GenWidth += 10;
                    break;

                case Keys.Subtract:
                    GenWidth -= 10;
                    break;

                case Keys.Left:
                    if (fRootPerson != null) {
                        GEDCOMIndividualRecord father, mother;
                        fRootPerson.GetParents(out father, out mother);

                        if (father != null) {
                            RootPerson = father;
                        }
                    }
                    break;

                case Keys.Right:
                    if (fRootPerson != null) {
                        GEDCOMIndividualRecord father, mother;
                        fRootPerson.GetParents(out father, out mother);

                        if (mother != null) {
                            RootPerson = mother;
                        }
                    }
                    break;

                case Keys.Back:
                    NavPrev();
                    return;

                default:
                    e.Handled = true;
                    break;
            }
        }
    }
}
