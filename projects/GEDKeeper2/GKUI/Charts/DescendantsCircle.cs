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
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Charts
{
    public class DescendantsCircle : CircleChart
    {
        private class PersonSegment : CircleSegment
        {
            public readonly List<PersonSegment> ChildSegments;
            public int TotalSubSegments;

            public PersonSegment(int generation) : base(generation)
            {
                ChildSegments = new List<PersonSegment>();
                TotalSubSegments = 0;
            }
        }


        public DescendantsCircle(IBaseWindow baseWin) : base(baseWin)
        {
        }

        protected override void BuildPathTree()
        {
            fSegments.Clear();

            // traverse tree
            fIndividualsCount = 0;

            PersonSegment rootSegment;
            if (fRootPerson != null) {
                rootSegment = TraverseDescendants(fRootPerson, 0);
            } else {
                return;
            }

            fCenterX = Width / 2 + fOffsetX;
            fCenterY = Height / 2 + fOffsetY;

            int inRad = CENTER_RAD - 50;
            float stepAngle = (360.0f / rootSegment.TotalSubSegments);

            CalcDescendants(rootSegment, inRad, -90.0f, stepAngle);
        }

        private void CalcDescendants(PersonSegment segment, int inRad, float startAngle, float stepAngle)
        {
            GraphicsPath path = segment.Path;

            int extRad;
            if (segment.Gen == 0) {
                segment.WedgeAngle = 360.0f;

                path.StartFigure();
                path.AddEllipse(fCenterX - inRad, fCenterY - inRad, inRad * 2, inRad * 2);
                path.CloseFigure();

                extRad = inRad;
            } else {
                extRad = inRad + fGenWidth;

                int size = (segment.TotalSubSegments > 0) ? segment.TotalSubSegments : 1;
                float wedgeAngle = stepAngle * size;

                segment.StartAngle = startAngle;
                segment.WedgeAngle = wedgeAngle;
                segment.Rad = inRad + 50;

                CreateCircleSegment(path, fCenterX, fCenterY, inRad, extRad, wedgeAngle, startAngle, startAngle + wedgeAngle);
            }

            float childStartAngle = startAngle;
            for (int i = 0; i < segment.ChildSegments.Count; i++) {
                PersonSegment childSegment = segment.ChildSegments[i];

                CalcDescendants(childSegment, extRad, childStartAngle, stepAngle);

                int steps = (childSegment.TotalSubSegments > 0) ? childSegment.TotalSubSegments : 1;

                childStartAngle += stepAngle * steps;
            }
        }

        private PersonSegment TraverseDescendants(GEDCOMIndividualRecord iRec, int gen)
        {
            if (iRec == null) return null;
            
            try
            {
                fIndividualsCount++;

                PersonSegment resultSegment = new PersonSegment(gen);
                resultSegment.IRec = iRec;
                fSegments.Add(resultSegment);

                if (gen < fMaxGenerations)
                {
                    int num2 = iRec.SpouseToFamilyLinks.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[j].Family;
                        if (GKUtils.IsRecordAccess(family.Restriction, fShieldState))
                        {
                            family.SortChilds();

                            int num3 = family.Childrens.Count;
                            for (int i = 0; i < num3; i++)
                            {
                                GEDCOMIndividualRecord child = family.Childrens[i].Value as GEDCOMIndividualRecord;
                                PersonSegment childSegment = TraverseDescendants(child, gen + 1);

                                int size = (childSegment.TotalSubSegments > 0) ? childSegment.TotalSubSegments : 1;
                                resultSegment.TotalSubSegments += size;

                                resultSegment.ChildSegments.Add(childSegment);
                            }
                        }
                    }
                }

                return resultSegment;
            }
            catch
            {
                return null;
            }
        }

        protected override void InternalDraw(Graphics gfx)
        {
            gfx.SmoothingMode = SmoothingMode.AntiAlias;

            int num = fSegments.Count;
            for (int i = 0; i < num; i++) {
                PersonSegment segment = (PersonSegment)fSegments[i];
                if (segment.IRec == null) continue;

                int brIndex = (segment.Gen == 0) ? 9 : segment.Gen - 1;
                SolidBrush brush = (fSelected == segment) ? fDarkBrushes[brIndex] : fCircleBrushes[brIndex];

                GraphicsPath path = segment.Path;
                gfx.FillPath(brush, path);
                gfx.DrawPath(fPen, path);
            }

            for (int i = 0; i < num; i++) {
                DrawPersonName(gfx, fSegments[i]);
            }
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
