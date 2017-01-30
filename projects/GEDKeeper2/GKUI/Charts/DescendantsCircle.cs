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

                fCenterX = Width / 2 + fOffsetX;
                fCenterY = Height / 2 + fOffsetY;

                const int inRad = CENTER_RAD - 50;
                float stepAngle = (360.0f / rootSegment.TotalSubSegments);

                CalcDescendants(rootSegment, inRad, -90.0f, stepAngle);
            }
        }

        private void CalcDescendants(PersonSegment segment, int inRad, float startAngle, float stepAngle)
        {
            GraphicsPath path = segment.Path;

            int ctX = fCenterX;
            int ctY = fCenterY;

            int extRad;
            if (segment.Gen == 0) {
                segment.WedgeAngle = 360.0f;

                path.StartFigure();
                path.AddEllipse(ctX - inRad, ctY - inRad, inRad * 2, inRad * 2);
                path.CloseFigure();

                extRad = inRad;
            } else {
                extRad = inRad + fGenWidth;

                int size = (segment.TotalSubSegments > 0) ? segment.TotalSubSegments : 1;
                float wedgeAngle = stepAngle * size;

                int ir2 = inRad * 2;
                int er2 = extRad * 2;

                float ang1 = startAngle;
                float angval1 = ang1 * PI / 180.0f;
                int px1 = ctX + (int)(inRad * Math.Cos(angval1));
                int py1 = ctY + (int)(inRad * Math.Sin(angval1));
                int px2 = ctX + (int)(extRad * Math.Cos(angval1));
                int py2 = ctY + (int)(extRad * Math.Sin(angval1));

                float ang2 = ang1 + wedgeAngle;
                float angval2 = ang2 * PI / 180.0f;
                int nx1 = ctX + (int)(inRad * Math.Cos(angval2));
                int ny1 = ctY + (int)(inRad * Math.Sin(angval2));
                int nx2 = ctX + (int)(extRad * Math.Cos(angval2));
                int ny2 = ctY + (int)(extRad * Math.Sin(angval2));

                segment.StartAngle = startAngle;
                segment.WedgeAngle = wedgeAngle;
                segment.Rad = inRad + 50;

                path.StartFigure();
                path.AddLine(px2, py2, px1, py1);
                path.AddArc(ctX - inRad, ctY - inRad, ir2, ir2, ang1, wedgeAngle);
                path.AddLine(nx1, ny1, nx2, ny2);
                path.AddArc(ctX - extRad, ctY - extRad, er2, er2, ang2, -wedgeAngle);
                path.CloseFigure();
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
                    int numberOfFamilyLinks = iRec.SpouseToFamilyLinks.Count;
                    for (int j = 0; j < numberOfFamilyLinks; j++)
                    {
                        GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[j].Family;
                        if (GKUtils.IsRecordAccess(family.Restriction, fShieldState))
                        {
                            family.SortChilds();

                            int numberOfChildren = family.Childrens.Count;
                            for (int i = 0; i < numberOfChildren; i++)
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

            int numberOfSegments = fSegments.Count;
            for (int i = 0; i < numberOfSegments; i++) {
                PersonSegment segment = (PersonSegment)fSegments[i];
                if (segment.IRec == null) continue;
                /* FIXME(brigadir15@gmail.com): Replace literal `9` below with a
                 * const. */
                int brIndex = (segment.Gen == 0) ? 9 : segment.Gen - 1;
                SolidBrush brush = (fSelected == segment) ? fDarkBrushes[brIndex] : fCircleBrushes[brIndex];

                GraphicsPath path = segment.Path;
                gfx.FillPath(brush, path);
                gfx.DrawPath(fPen, path);
            }
            for (int i = 0; i < numberOfSegments; i++) {
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
