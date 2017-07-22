/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Charts
{
    public delegate void ARootChangedEventHandler(object sender, GEDCOMIndividualRecord person);

    public enum CircleChartType { Ancestors, Descendants }

    public abstract class CircleSegment : BaseObject
    {
        public int Gen;
        public GEDCOMIndividualRecord IRec;
        public IGfxPath Path;

        public float Rad; // ?
        public float IntRad; // Internal radius
        public float ExtRad; // External radius

        public float StartAngle;
        public float WedgeAngle;

        protected CircleSegment(int generation)
        {
            Gen = generation;
            IRec = null;
            Path = AppHost.GfxProvider.CreatePath();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (Path != null) Path.Dispose();
            }
            base.Dispose(disposing);
        }
    }

    public sealed class AncPersonSegment : CircleSegment
    {
        public int GroupIndex;
        public AncPersonSegment FatherSegment;
        public AncPersonSegment MotherSegment;

        public AncPersonSegment(int generation) : base(generation)
        {
            GroupIndex = -1;
            FatherSegment = null;
            MotherSegment = null;
        }
    }

    public sealed class DescPersonSegment : CircleSegment
    {
        public readonly List<DescPersonSegment> ChildSegments;
        public int TotalSubSegments;

        public DescPersonSegment(int generation) : base(generation)
        {
            ChildSegments = new List<DescPersonSegment>();
            TotalSubSegments = 0;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class CircleChartModel : ChartModel
    {
        public const int CENTRAL_INDEX = 9;
        public const float CENTER_RAD = 90;
        public const float DEFAULT_GEN_WIDTH = 60;

        private readonly IBrush[] fCircleBrushes;
        private readonly IBrush[] fDarkBrushes;

        private IBaseWindow fBase;
        private ExtRectF fBounds;
        private float fGenWidth;
        private int fIndividualsCount;
        /* TODO(brigadir15@gmail.com): Member `fMaxGenerations` should be a
         * const field, I believe. Member `CircleSegment::Gen` is used as an
         * index when accessing the array `fCircleBrushes`, for example. The
         * latter contains **predefined** brush list, with first eight elements
         * used as brushes for eight possible generations (`CircleChart` ctor
         * assigns `fMaxGenerations` with `8`; see code below).
         * Thus, if one will want to extent number of available visible
         * generation, he/she will have to change a huge amount of the code.
         * Therefore, to avoid someone's tendency to change initial values of
         * member `fMaxGenerations`, the member should be a const field. */
        private int fMaxGenerations;
        private AncestorsCircleOptions fOptions;
        private IPen fPen;
        private GEDCOMIndividualRecord fRootPerson;
        private readonly List<CircleSegment> fSegments;
        private CircleSegment fSelected;
        private ShieldState fShieldState;

        #region Only ancestors circle
        private int fGroupCount;
        private bool fGroupsMode;
        #endregion

        public IFont Font;


        public IBaseWindow Base
        {
            get { return fBase; }
            set {
                fBase = value;
                fShieldState = fBase.Context.ShieldState;
            }
        }

        public ExtRectF Bounds
        {
            get { return fBounds; }
        }

        public float GenWidth
        {
            get {
                return fGenWidth;
            }
            set {
                if (value < 20 || value > 100) return;

                fGenWidth = value;
            }
        }

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
                    GenWidth = CircleChartModel.DEFAULT_GEN_WIDTH;
                }
            }
        }

        public int IndividualsCount
        {
            get { return fIndividualsCount; }
        }

        public AncestorsCircleOptions Options
        {
            get { return fOptions; }
            set { fOptions = value; }
        }

        public float PenWidth
        {
            get { return fPen.Width; }
        }

        public GEDCOMIndividualRecord RootPerson
        {
            get {
                return fRootPerson;
            }
            set {
                if (fRootPerson != value) {
                    fRootPerson = value;
                }
            }
        }

        public List<CircleSegment> Segments
        {
            get { return fSegments; }
        }

        public CircleSegment Selected
        {
            get { return fSelected; }
            set { fSelected = value; }
        }


        public CircleChartModel()
        {
            fCircleBrushes = new IBrush[AncestorsCircleOptions.MAX_BRUSHES];
            fDarkBrushes = new IBrush[AncestorsCircleOptions.MAX_BRUSHES];

            fBounds = new ExtRectF();
            fGenWidth = CircleChartModel.DEFAULT_GEN_WIDTH;
            fMaxGenerations = 8;
            fSegments = new List<CircleSegment>();
            fSelected = null;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                DisposeBrushes();
            }
            base.Dispose(disposing);
        }

        public void CreateBrushes()
        {
            for (int i = 0; i < fOptions.BrushColor.Length; i++)
            {
                IColor col = fOptions.BrushColor[i];

                fCircleBrushes[i] = fRenderer.CreateSolidBrush(col);
                fDarkBrushes[i] = fRenderer.CreateSolidBrush(col.Darker(0.2f));
            }

            fPen = fRenderer.CreatePen(fOptions.BrushColor[10], 1.0f);
        }

        public void DisposeBrushes()
        {
            for (int i = 0; i < fOptions.BrushColor.Length; i++)
            {
                if (fCircleBrushes[i] != null) fCircleBrushes[i].Dispose();
                if (fDarkBrushes[i] != null) fDarkBrushes[i].Dispose();
            }

            if (fPen != null) fPen.Dispose();
        }

        public void AdjustBounds()
        {
            /* Update scrolling area. */
            fBounds.Left = 0.0f;
            fBounds.Top = 0.0f;
            fBounds.Right = 0.0f;
            fBounds.Bottom = 0.0f;
            foreach (var segment in fSegments) {
                ExtRectF bound = segment.Path.GetBounds();
                fBounds.Left = Math.Min(fBounds.Left, bound.Left);
                fBounds.Top = Math.Min(fBounds.Top, bound.Top);
                fBounds.Right = Math.Max(fBounds.Right, bound.Right);
                fBounds.Bottom = Math.Max(fBounds.Bottom, bound.Bottom);
            }

            /* Add double width of the pen -- adjust both sides. */
            fImageHeight = (int)(fBounds.GetHeight() + fPen.Width * 2);
            fImageWidth = (int)(fBounds.GetWidth() + fPen.Width * 2);
        }

        public CircleSegment FindSegmentByRec(GEDCOMIndividualRecord iRec)
        {
            CircleSegment result = null;

            int numberOfSegments = fSegments.Count;
            for (int i = 0; i < numberOfSegments; i++) {
                CircleSegment segment = fSegments[i];

                if (segment.IRec == iRec) {
                    result = segment;
                    break;
                }
            }

            return result;
        }

        public CircleSegment FindSegment(float dX, float dY)
        {
            double rad = Math.Sqrt(dX * dX + dY * dY);
            double angle = SysUtils.RadiansToDegrees(Math.Atan2(dY, dX));
            if (angle <= -90) angle += 360.0f;

            CircleSegment result = null;

            int numberOfSegments = fSegments.Count;
            for (int i = 0; i < numberOfSegments; i++)
            {
                CircleSegment segment = fSegments[i];
                double startAng = segment.StartAngle;
                double endAng = startAng + segment.WedgeAngle;

                if ((segment.IntRad <= rad && rad < segment.ExtRad) &&
                    (startAng <= angle && angle < endAng))
                {
                    result = segment;
                    break;
                }
            }

            return result;
        }

        /// <summary>
        /// Renders a specified <paramref name="segment"/>'s person name within
        /// the segment.
        /// </summary>
        /// <param name="gfx">GDI+ context to render on. This member may change
        /// this context's transformation. If it does, it also reverts the
        /// transformation back. Thus, from the point of view of the client code
        /// this member doesn't change the context's transformation.</param>
        /// <param name="segment">Source segment to be drawn on `gfx`.</param>
        private void DrawPersonName(CircleSegment segment)
        {
            int gen = segment.Gen;
            GEDCOMIndividualRecord iRec = segment.IRec;

            string surn, givn;
            if (iRec == null) {
                if (gen == 0) {
                    givn = "Choose";
                    surn = "subject";
                } else {
                    return;
                }
            } else {
                var parts = GKUtils.GetNameParts(iRec);
                surn = parts.Surname;
                givn = parts.Name;
            }

            ExtSizeF size;
            float rad = segment.Rad - 20;
            float angle = segment.StartAngle + 90.0f + segment.WedgeAngle / 2;
            float wedgeAngle = segment.WedgeAngle;

            bool isNarrow = IsNarrowSegment(givn, rad, wedgeAngle, Font);

            object mtx = fRenderer.SaveTransform();

            if (gen == 0) {

                size = fRenderer.GetTextSize(surn, Font);
                fRenderer.DrawString(surn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f - size.Height / 2f);
                size = fRenderer.GetTextSize(givn, Font);
                fRenderer.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, 0f);

            } else {

                if (isNarrow) {

                    float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                    float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                    fRenderer.TranslateTransform(dx, -dy);
                    fRenderer.RotateTransform(angle - 90.0f);

                    size = fRenderer.GetTextSize(givn, Font);
                    fRenderer.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);

                } else {

                    if (wedgeAngle < 20) {

                        float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                        float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                        fRenderer.TranslateTransform(dx, -dy);
                        fRenderer.RotateTransform(angle);

                        size = fRenderer.GetTextSize(givn, Font);
                        fRenderer.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);

                    } else if (wedgeAngle < 180) {

                        if (fOptions.ArcText) {
                            if (gen == 2) {
                                size = fRenderer.GetTextSize(surn, Font);
                                fRenderer.DrawArcText(surn, 0.0f, 0.0f, rad + size.Height / 2f,
                                                      segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);

                                size = fRenderer.GetTextSize(givn, Font);
                                fRenderer.DrawArcText(givn, 0.0f, 0.0f, rad - size.Height / 2f,
                                                      segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);
                            } else {
                                fRenderer.DrawArcText(givn, 0.0f, 0.0f, rad,
                                                      segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);
                            }
                        } else {
                            float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                            float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                            fRenderer.TranslateTransform(dx, -dy);
                            fRenderer.RotateTransform(angle);

                            size = fRenderer.GetTextSize(surn, Font);
                            fRenderer.DrawString(surn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);

                            size = fRenderer.GetTextSize(givn, Font);
                            dx = (float)Math.Sin(Math.PI * angle / 180.0f) * (rad - size.Height);
                            dy = (float)Math.Cos(Math.PI * angle / 180.0f) * (rad - size.Height);

                            fRenderer.RestoreTransform(mtx);
                            mtx = fRenderer.SaveTransform();

                            fRenderer.TranslateTransform(dx, -dy);
                            fRenderer.RotateTransform(angle);

                            fRenderer.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);
                        }

                    } else if (wedgeAngle < 361) {

                        if (fOptions.ArcText) {
                            size = fRenderer.GetTextSize(surn, Font);
                            fRenderer.DrawArcText(surn, 0.0f, 0.0f, rad + size.Height / 2f,
                                                  segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);

                            size = fRenderer.GetTextSize(givn, Font);
                            fRenderer.DrawArcText(givn, 0.0f, 0.0f, rad - size.Height / 2f,
                                                  segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);
                        } else {
                            float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                            float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                            fRenderer.TranslateTransform(dx, -dy);
                            fRenderer.RotateTransform(angle);

                            size = fRenderer.GetTextSize(surn, Font);
                            fRenderer.DrawString(surn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);
                            size = fRenderer.GetTextSize(givn, Font);
                            fRenderer.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f + size.Height);
                        }

                    }
                }
            }

            fRenderer.RestoreTransform(mtx);
        }

        private bool IsNarrowSegment(string text, float radius, float wedgeAngle, IFont font)
        {
            ExtSizeF size = fRenderer.GetTextSize(text, font);
            radius = radius + size.Height / 2.0f;

            float wedgeL = radius * (float)SysUtils.DegreesToRadians(wedgeAngle);

            return (wedgeL / size.Width <= 0.9f);
        }

        #region Ancestors Circle

        public void BuildAncTree()
        {
            fSegments.Clear();

            const float startRad = CircleChartModel.CENTER_RAD - 50;
            float inRad = startRad;

            AncPersonSegment segment = new AncPersonSegment(0);
            segment.IntRad = 0;
            segment.ExtRad = inRad;
            segment.StartAngle = 0 - 90.0f;
            segment.WedgeAngle = segment.StartAngle + 360.0f;
            IGfxPath path = segment.Path;
            path.StartFigure();
            path.AddEllipse(-inRad, -inRad, inRad * 2.0f, inRad * 2.0f);
            path.CloseFigure();
            fSegments.Add(segment);

            int maxSteps = 1;
            for (int gen = 1; gen <= fMaxGenerations; gen++) {
                inRad = startRad + ((gen - 1) * fGenWidth);
                float extRad = inRad + fGenWidth;

                maxSteps *= 2;
                float stepAngle = (360.0f / maxSteps);

                for (int step = 0; step < maxSteps; step++)
                {
                    float ang1 = (step * stepAngle) - 90.0f;
                    float ang2 = ang1 + stepAngle;

                    segment = new AncPersonSegment(gen);
                    segment.StartAngle = ang1;
                    segment.WedgeAngle = stepAngle;
                    fRenderer.CreateCircleSegment(segment.Path, inRad, extRad, stepAngle, ang1, ang2);
                    fSegments.Add(segment);
                }
            }

            // traverse tree
            fGroupCount = -1;
            fIndividualsCount = 0;
            if (fRootPerson == null) return;

            fIndividualsCount++;
            AncPersonSegment rootSegment = SetSegmentParams(0, fRootPerson, 0, -1);
            if (rootSegment == null) return;

            rootSegment.WedgeAngle = 360.0f;

            GEDCOMIndividualRecord father, mother;
            fRootPerson.GetParents(out father, out mother);

            if (mother != null) {
                rootSegment.MotherSegment = TraverseAncestors(mother, 90f, 1, CircleChartModel.CENTER_RAD, 90.0f, 1, -1);
            }

            if (father != null) {
                rootSegment.FatherSegment = TraverseAncestors(father, 270.0f, 1, CircleChartModel.CENTER_RAD, 90.0f, 1, -1);
            }
        }

        private AncPersonSegment SetSegmentParams(int index, GEDCOMIndividualRecord rec, float rad, int groupIndex)
        {
            if (index < 0 || index >= fSegments.Count) {
                return null;
            }

            AncPersonSegment segment = (AncPersonSegment)fSegments[index];
            segment.IRec = rec;
            segment.Rad = rad;
            segment.GroupIndex = groupIndex;
            return segment;
        }

        private AncPersonSegment TraverseAncestors(GEDCOMIndividualRecord iRec, float v, int gen, float rad, float ro, int prevSteps, int groupIndex)
        {
            try
            {
                fIndividualsCount++;

                if (fGroupsMode && groupIndex == -1) {
                    AncPersonSegment otherSegment = (AncPersonSegment)FindSegmentByRec(iRec);
                    if (otherSegment != null) {
                        fGroupCount++;
                        groupIndex = fGroupCount;
                        TraverseGroups(otherSegment, groupIndex);
                    }
                }

                int genSize = 1 << gen;
                float ang = (360.0f / genSize);

                int idx = prevSteps + (int)(v / ang);
                AncPersonSegment segment = SetSegmentParams(idx, iRec, rad, groupIndex);

                if (segment != null && gen < fMaxGenerations)
                {
                    float inRad = rad;
                    float extRad = rad + fGenWidth;

                    segment.IntRad = inRad - 50;
                    segment.ExtRad = extRad - 50;

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

        private static void TraverseGroups(AncPersonSegment segment, int groupIndex)
        {
            if (segment == null) return;

            segment.GroupIndex = groupIndex;
            if (segment.FatherSegment != null) TraverseGroups(segment.FatherSegment, groupIndex);
            if (segment.MotherSegment != null) TraverseGroups(segment.MotherSegment, groupIndex);
        }

        public void DrawAncestors()
        {
            int numberOfSegments = fSegments.Count;
            for (int i = 0; i < numberOfSegments; i++) {
                AncPersonSegment segment = (AncPersonSegment)fSegments[i];

                bool draw = (!Options.HideEmptySegments || segment.IRec != null);

                if (draw) {
                    int brIndex;
                    if (fGroupsMode) {
                        brIndex = (segment.GroupIndex == -1) ? 11 : segment.GroupIndex;
                    } else {
                        brIndex = (segment.Gen == 0) ? CircleChartModel.CENTRAL_INDEX : segment.Gen - 1;
                    }

                    IBrush brush = (fSelected == segment) ? fDarkBrushes[brIndex] : fCircleBrushes[brIndex];

                    IGfxPath path = segment.Path;
                    fRenderer.FillPath(brush, path);
                    fRenderer.DrawPath(fPen, path);
                }

                DrawPersonName(fSegments[i]);
            }
        }

        #endregion

        #region Descendants Circle

        public void BuildDescTree()
        {
            fSegments.Clear();
            fIndividualsCount = 0;
            if (fRootPerson == null) return;

            // traverse tree
            DescPersonSegment rootSegment = TraverseDescendants(fRootPerson, 0);
            if (rootSegment == null) return;

            const float inRad = CircleChartModel.CENTER_RAD - 50;
            float stepAngle = (360.0f / rootSegment.TotalSubSegments);

            CalcDescendants(rootSegment, inRad, -90.0f, stepAngle);
        }

        private void CalcDescendants(DescPersonSegment segment, float inRad, float startAngle, float stepAngle)
        {
            IGfxPath path = segment.Path;

            float extRad;
            if (segment.Gen == 0) {
                segment.StartAngle = startAngle;
                segment.WedgeAngle = 360.0f;
                segment.IntRad = 0;
                segment.ExtRad = inRad;

                path.StartFigure();
                path.AddEllipse(-inRad, -inRad, inRad * 2.0f, inRad * 2.0f);
                path.CloseFigure();

                extRad = inRad;
            } else {
                extRad = inRad + fGenWidth;

                int size = Math.Max(1, segment.TotalSubSegments);
                float wedgeAngle = stepAngle * size;

                segment.StartAngle = startAngle;
                segment.WedgeAngle = wedgeAngle;
                segment.Rad = inRad + 50;
                segment.IntRad = inRad;
                segment.ExtRad = extRad;

                fRenderer.CreateCircleSegment(path, inRad, extRad, wedgeAngle, startAngle, startAngle + wedgeAngle);
            }

            for (int i = 0; i < segment.ChildSegments.Count; i++) {
                DescPersonSegment childSegment = segment.ChildSegments[i];

                CalcDescendants(childSegment, extRad, startAngle, stepAngle);

                int steps = Math.Max(1, childSegment.TotalSubSegments);

                startAngle += stepAngle * steps;
            }
        }

        private DescPersonSegment TraverseDescendants(GEDCOMIndividualRecord iRec, int gen)
        {
            if (iRec == null) return null;
            
            try
            {
                fIndividualsCount++;

                DescPersonSegment resultSegment = new DescPersonSegment(gen);
                resultSegment.IRec = iRec;
                fSegments.Add(resultSegment);

                if (gen < fMaxGenerations)
                {
                    int numberOfFamilyLinks = iRec.SpouseToFamilyLinks.Count;
                    for (int j = 0; j < numberOfFamilyLinks; j++)
                    {
                        GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[j].Family;
                        if (!fBase.Context.IsRecordAccess(family.Restriction)) continue;

                        family.SortChilds();

                        int numberOfChildren = family.Children.Count;
                        for (int i = 0; i < numberOfChildren; i++)
                        {
                            GEDCOMIndividualRecord child = family.Children[i].Value as GEDCOMIndividualRecord;
                            DescPersonSegment childSegment = TraverseDescendants(child, gen + 1);

                            int size = Math.Max(1, childSegment.TotalSubSegments);
                            resultSegment.TotalSubSegments += size;

                            resultSegment.ChildSegments.Add(childSegment);
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

        public void DrawDescendants()
        {
            int numberOfSegments = fSegments.Count;
            for (int i = 0; i < numberOfSegments; i++) {
                DescPersonSegment segment = (DescPersonSegment)fSegments[i];
                if (segment.IRec == null) continue;

                int brIndex = (segment.Gen == 0) ? CircleChartModel.CENTRAL_INDEX : segment.Gen - 1;
                IBrush brush = (fSelected == segment) ? fDarkBrushes[brIndex] : fCircleBrushes[brIndex];

                IGfxPath path = segment.Path;
                fRenderer.FillPath(brush, path);
                fRenderer.DrawPath(fPen, path);
                DrawPersonName(fSegments[i]);
            }
        }

        #endregion
    }
}
