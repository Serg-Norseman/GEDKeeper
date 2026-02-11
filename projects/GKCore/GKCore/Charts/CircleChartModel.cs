/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Options;

namespace GKCore.Charts
{
    public delegate void ARootChangedEventHandler(object sender, GDMIndividualRecord person);

    public enum CircleChartType { Ancestors, Descendants }

    public abstract class CircleSegment : BaseObject
    {
        public int Gen;
        public GDMIndividualRecord IRec;
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
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
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
        public GDMSex ParentLine;

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
        public const int MAX_GENERATIONS = 8;

        private readonly IBrush[] fCircleBrushes;
        private readonly IBrush[] fDarkBrushes;
        public IColor fFemaleColor;
        public IColor fMaleColor;

        private IBaseWindow fBase;
        private ExtRectF fBounds;
        private float fGenWidth;
        private int fIndividualsCount;
        private CircleChartOptions fOptions;
        private IPen fPen;
        private GDMIndividualRecord fRootPerson;
        private readonly List<CircleSegment> fSegments;
        private CircleSegment fSelected;
        private int fVisibleGenerations;

        #region Only ancestors circle
        private bool fFanMode;
        private int fGroupCount;
        private bool fGroupsMode;
        private bool fParentsColors;
        #endregion

        public IFont Font;


        public IBaseWindow Base
        {
            get { return fBase; }
            set { fBase = value; }
        }

        public ExtRectF Bounds
        {
            get { return fBounds; }
        }

        public bool FanMode
        {
            get {
                return fFanMode;
            }
            set {
                fFanMode = value;
            }
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
                    fVisibleGenerations = MAX_GENERATIONS;
                    GenWidth = 60;
                } else {
                    fVisibleGenerations = MAX_GENERATIONS;
                    GenWidth = CircleChartModel.DEFAULT_GEN_WIDTH;
                }
            }
        }

        public int IndividualsCount
        {
            get { return fIndividualsCount; }
        }

        public CircleChartOptions Options
        {
            get { return fOptions; }
            set { fOptions = value; }
        }

        public bool ParentsColors
        {
            get { return fParentsColors; }
            set { fParentsColors = value; }
        }

        public float PenWidth
        {
            get { return fPen.Width; }
        }

        public GDMIndividualRecord RootPerson
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

        public int VisibleGenerations
        {
            get { return fVisibleGenerations; }
            set {
                if (value >= 1 && value <= MAX_GENERATIONS) {
                    fVisibleGenerations = value;
                }
            }
        }


        public CircleChartModel()
        {
            fCircleBrushes = new IBrush[CircleChartOptions.MAX_BRUSHES];
            fDarkBrushes = new IBrush[CircleChartOptions.MAX_BRUSHES];

            fBounds = new ExtRectF();
            fFanMode = false;
            fGenWidth = CircleChartModel.DEFAULT_GEN_WIDTH;
            fParentsColors = false;
            fSegments = new List<CircleSegment>();
            fSelected = null;
            fVisibleGenerations = MAX_GENERATIONS;
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
            for (int i = 0; i < fOptions.BrushColor.Length; i++) {
                IColor col = fOptions.BrushColor[i];

                fCircleBrushes[i] = fRenderer.CreateBrush(col);
                fDarkBrushes[i] = fRenderer.CreateBrush(col.Darker(0.2f));
            }

            fFemaleColor = ChartRenderer.GetColor(TreeChartOptions.FEMALE_COLOR).Darker(0.4f);
            fMaleColor = ChartRenderer.GetColor(TreeChartOptions.MALE_COLOR).Darker(0.4f);

            fPen = fRenderer.CreatePen(fOptions.BrushColor[10], 1.0f);
        }

        public void DisposeBrushes()
        {
            for (int i = 0; i < fOptions.BrushColor.Length; i++) {
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

        public CircleSegment FindSegmentByRec(GDMIndividualRecord iRec)
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
            double angle = MathHelper.RadiansToDegrees(Math.Atan2(dY, dX));
            if (angle <= -90) angle += 360.0f;

            CircleSegment result = null;

            int numberOfSegments = fSegments.Count;
            for (int i = 0; i < numberOfSegments; i++) {
                CircleSegment segment = fSegments[i];
                double startAng = segment.StartAngle;
                double endAng = startAng + segment.WedgeAngle;

                if ((segment.IntRad <= rad && rad < segment.ExtRad) && (startAng <= angle && angle < endAng)) {
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
        /// <param name="segment">Source segment to be drawn on `gfx`.</param>
        private void DrawPersonName(CircleSegment segment)
        {
            int gen = segment.Gen;
            GDMIndividualRecord iRec = segment.IRec;

            string surn, givn;
            if (iRec == null) {
                if (gen == 0) {
                    givn = "Choose";
                    surn = "subject";
                } else {
                    return;
                }
            } else {
                var parts = GKUtils.GetNameParts(fBase.Context.Tree, iRec);
                surn = parts.Surname;
                givn = parts.Name;
            }

            var brush = fCircleBrushes[8];
            ExtSizeF size;
            float rad = segment.Rad - 20;
            float angle = segment.StartAngle + 90.0f + segment.WedgeAngle / 2;
            float wedgeAngle = segment.WedgeAngle;

            bool isNarrow = IsNarrowSegment(givn, rad, wedgeAngle, Font);

            fRenderer.SaveTransform();

            if (gen == 0) {

                // central circle
                size = fRenderer.GetTextSize(surn, Font);
                fRenderer.DrawString(surn, Font, brush, -size.Width / 2f, -size.Height / 2f - size.Height / 2f);
                size = fRenderer.GetTextSize(givn, Font);
                fRenderer.DrawString(givn, Font, brush, -size.Width / 2f, 0f);

            } else {

                if (isNarrow) {
                    //var debugBrush = fRenderer.CreateSolidBrush(ChartRenderer.Red);

                    // narrow segments of 6-8 generations, radial text
                    float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                    float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                    fRenderer.TranslateTransform(dx, -dy);

                    if (fOptions.LTRCorrection && (angle >= 180 && angle < 360)) {
                        angle -= 180.0f;
                    }

                    fRenderer.RotateTransform(angle - 90.0f);

                    size = fRenderer.GetTextSize(givn, Font);
                    fRenderer.DrawString(givn, Font, brush, -size.Width / 2f, -size.Height / 2f);

                } else {

                    if (wedgeAngle < 20) {

                        float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                        float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                        fRenderer.TranslateTransform(dx, -dy);
                        fRenderer.RotateTransform(angle);

                        size = fRenderer.GetTextSize(givn, Font);
                        fRenderer.DrawString(givn, Font, brush, -size.Width / 2f, -size.Height / 2f);

                    } else if (wedgeAngle < 180) {

                        if (fOptions.ArcText) {
                            if (gen == 2) {
                                size = fRenderer.GetTextSize(surn, Font);
                                fRenderer.DrawArcText(surn, 0.0f, 0.0f, rad + size.Height / 2f,
                                                      segment.StartAngle, segment.WedgeAngle, true, true, Font, brush);

                                size = fRenderer.GetTextSize(givn, Font);
                                fRenderer.DrawArcText(givn, 0.0f, 0.0f, rad - size.Height / 2f,
                                                      segment.StartAngle, segment.WedgeAngle, true, true, Font, brush);
                            } else {
                                fRenderer.DrawArcText(givn, 0.0f, 0.0f, rad,
                                                      segment.StartAngle, segment.WedgeAngle, true, true, Font, brush);
                            }
                        } else {
                            float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                            float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                            fRenderer.TranslateTransform(dx, -dy);
                            fRenderer.RotateTransform(angle);

                            size = fRenderer.GetTextSize(surn, Font);
                            fRenderer.DrawString(surn, Font, brush, -size.Width / 2f, -size.Height / 2f);

                            size = fRenderer.GetTextSize(givn, Font);
                            dx = (float)Math.Sin(Math.PI * angle / 180.0f) * (rad - size.Height);
                            dy = (float)Math.Cos(Math.PI * angle / 180.0f) * (rad - size.Height);

                            fRenderer.RestoreTransform();
                            fRenderer.SaveTransform();

                            fRenderer.TranslateTransform(dx, -dy);
                            fRenderer.RotateTransform(angle);

                            fRenderer.DrawString(givn, Font, brush, -size.Width / 2f, -size.Height / 2f);
                        }

                    } else if (wedgeAngle < 361) {

                        if (fOptions.ArcText) {
                            size = fRenderer.GetTextSize(surn, Font);
                            fRenderer.DrawArcText(surn, 0.0f, 0.0f, rad + size.Height / 2f,
                                                  segment.StartAngle, segment.WedgeAngle, true, true, Font, brush);

                            size = fRenderer.GetTextSize(givn, Font);
                            fRenderer.DrawArcText(givn, 0.0f, 0.0f, rad - size.Height / 2f,
                                                  segment.StartAngle, segment.WedgeAngle, true, true, Font, brush);
                        } else {
                            float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                            float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                            fRenderer.TranslateTransform(dx, -dy);
                            fRenderer.RotateTransform(angle);

                            size = fRenderer.GetTextSize(surn, Font);
                            fRenderer.DrawString(surn, Font, brush, -size.Width / 2f, -size.Height / 2f);
                            size = fRenderer.GetTextSize(givn, Font);
                            fRenderer.DrawString(givn, Font, brush, -size.Width / 2f, -size.Height / 2f + size.Height);
                        }

                    }
                }
            }

            fRenderer.RestoreTransform();
        }

        private bool IsNarrowSegment(string text, float radius, float wedgeAngle, IFont font)
        {
            ExtSizeF size = fRenderer.GetTextSize(text, font);
            radius += size.Height / 2.0f;

            float wedgeL = radius * (float)MathHelper.DegreesToRadians(wedgeAngle);

            return (wedgeL / size.Width <= 0.9f);
        }

        private void DefineSegment(CircleSegment segment, float rad, float inRad, float extRad, float startAngle, float wedgeAngle)
        {
            segment.StartAngle = startAngle;
            segment.WedgeAngle = wedgeAngle;
            segment.Rad = rad;
            segment.IntRad = inRad;
            segment.ExtRad = extRad;

            if (wedgeAngle == 360.0f) {
                segment.Path = fRenderer.CreateCirclePath(-extRad, -extRad, extRad * 2.0f, extRad * 2.0f);
            } else {
                segment.Path = fRenderer.CreateCircleSegmentPath(0, 0, inRad, extRad, wedgeAngle, startAngle, startAngle + wedgeAngle);
            }
        }

        private void DrawSegment(CircleSegment segment, IPen pen, IBrush brush)
        {
            fRenderer.DrawPath(pen, brush, segment.Path);
            DrawPersonName(segment);
        }

        #region Ancestors Circle

        public void BuildAncTree()
        {
            fSegments.Clear();

            const float startRad = CircleChartModel.CENTER_RAD - 50;
            float inRad = startRad;

            AncPersonSegment segment = new AncPersonSegment(0);
            DefineSegment(segment, 0, 0, inRad, 0 - 90.0f, 360.0f);
            fSegments.Add(segment);

            float fullAngle, zeroAngle, hcMth, hcFth, ro;
            if (!fFanMode) {
                fullAngle = 360.0f;
                zeroAngle = 0 - 90.0f;

                ro = 90.0f;
                hcMth = 90.0f;
                hcFth = 270.0f;
            } else {
                fullAngle = 240.0f;
                zeroAngle = 0 - 90.0f - fullAngle / 2;

                ro = 60.0f;
                hcMth = 60.0f;
                hcFth = 180.0f;
            }

            int maxSteps = 1;
            for (int gen = 1; gen <= fVisibleGenerations; gen++) {
                inRad = startRad + ((gen - 1) * fGenWidth);
                float extRad = inRad + fGenWidth;

                maxSteps *= 2;
                float wedgeAngle = (fullAngle / maxSteps);

                for (int step = 0; step < maxSteps; step++) {
                    float startAngle = zeroAngle + (step * wedgeAngle);

                    segment = new AncPersonSegment(gen);
                    DefineSegment(segment, 0, inRad, extRad, startAngle, wedgeAngle);
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

            GDMIndividualRecord father = null, mother = null;
            GDMFamilyRecord fam = fBase.Context.Tree.GetParentsFamily(fRootPerson);
            if (fam != null && fBase.Context.IsRecordAccess(fam.Restriction)) {
                fBase.Context.Tree.GetSpouses(fam, out father, out mother);
            }

            if (mother != null) {
                rootSegment.MotherSegment = TraverseAncestors(mother, fullAngle, hcMth, 1, CircleChartModel.CENTER_RAD, ro, 1, -1, GDMSex.svFemale);
            }

            if (father != null) {
                rootSegment.FatherSegment = TraverseAncestors(father, fullAngle, hcFth, 1, CircleChartModel.CENTER_RAD, ro, 1, -1, GDMSex.svMale);
            }
        }

        private AncPersonSegment SetSegmentParams(int index, GDMIndividualRecord rec, float rad, int groupIndex)
        {
            if (index < 0 || index >= fSegments.Count) {
                return null;
            }

            AncPersonSegment segment = (AncPersonSegment)fSegments[index];
            segment.IRec = rec;
            segment.Rad = rad; // it is necessary for text positioning
            segment.GroupIndex = groupIndex;
            return segment;
        }

        private AncPersonSegment TraverseAncestors(GDMIndividualRecord iRec, float fullAngle, float v, int gen, float rad, float ro, int prevSteps,
            int groupIndex, GDMSex parentLine)
        {
            try {
                fIndividualsCount++;

                if (fGroupsMode && groupIndex == -1) {
                    AncPersonSegment otherSegment = (AncPersonSegment)FindSegmentByRec(iRec);
                    if (otherSegment != null) {
                        fGroupCount++;
                        groupIndex = fGroupCount;
                        TraverseGroups(otherSegment, groupIndex);
                    }
                }

                // number of people in a generation
                int genSize = 1 << gen;

                // segment angle per person
                float ang = (fullAngle / genSize);

                // segment index
                int idx = prevSteps + (int)(v / ang);

                AncPersonSegment segment = SetSegmentParams(idx, iRec, rad, groupIndex);

                if (segment != null && gen < fVisibleGenerations) {
                    segment.ParentLine = parentLine;

                    GDMIndividualRecord father = null, mother = null;
                    GDMFamilyRecord fam = fBase.Context.Tree.GetParentsFamily(iRec);
                    if (fam != null && fBase.Context.IsRecordAccess(fam.Restriction)) {
                        fBase.Context.Tree.GetSpouses(fam, out father, out mother);
                    }

                    int nextSteps = prevSteps + genSize;

                    v -= (Math.Abs(ang - ro) / 2.0f);
                    if (father != null) {
                        segment.FatherSegment = TraverseAncestors(father, fullAngle, v, gen + 1, rad + fGenWidth, ro / 2.0f, nextSteps, groupIndex, parentLine);
                    }

                    v += (ang / 2.0f);
                    if (mother != null) {
                        segment.MotherSegment = TraverseAncestors(mother, fullAngle, v, gen + 1, rad + fGenWidth, ro / 2.0f, nextSteps, groupIndex, parentLine);
                    }
                }

                return segment;
            } catch {
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

                bool draw = (!fOptions.HideEmptySegments || segment.IRec != null);

                if (draw) {
                    int brIndex;
                    if (fGroupsMode) {
                        brIndex = (segment.GroupIndex == -1) ? 11 : segment.GroupIndex;
                    } else {
                        brIndex = (segment.Gen == 0) ? CircleChartModel.CENTRAL_INDEX : segment.Gen - 1;
                    }

                    bool brushDispose;
                    IBrush brush;
                    if (!fParentsColors) {
                        brush = (fSelected == segment) ? fDarkBrushes[brIndex] : fCircleBrushes[brIndex];
                        brushDispose = false;
                    } else {
                        IColor color;
                        if (segment.ParentLine == GDMSex.svFemale) {
                            color = fFemaleColor;
                        } else {
                            color = fMaleColor;
                        }

                        brush = fRenderer.CreateBrush(color.Lighter(0.15f * segment.Gen));
                        brushDispose = true;
                    }

                    DrawSegment(segment, fPen, brush);

                    if (brushDispose) {
                        brush.Dispose();
                    }
                }
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
            float extRad;
            if (segment.Gen == 0) {
                DefineSegment(segment, 0, 0, inRad, startAngle, 360.0f);

                extRad = inRad;
            } else {
                extRad = inRad + fGenWidth;

                int size = Math.Max(1, segment.TotalSubSegments);
                float wedgeAngle = stepAngle * size;

                // in Eto.Drawings 360 degrees for the segments
                // leads to a crash of drawing
                if (wedgeAngle == 360.0f) {
                    wedgeAngle -= 0.1f;
                }

                DefineSegment(segment, inRad + 50, inRad, extRad, startAngle, wedgeAngle);
            }

            for (int i = 0; i < segment.ChildSegments.Count; i++) {
                DescPersonSegment childSegment = segment.ChildSegments[i];

                CalcDescendants(childSegment, extRad, startAngle, stepAngle);

                int steps = Math.Max(1, childSegment.TotalSubSegments);
                startAngle += stepAngle * steps;
            }
        }

        private DescPersonSegment TraverseDescendants(GDMIndividualRecord iRec, int gen)
        {
            if (iRec == null) return null;
            
            try {
                fIndividualsCount++;

                DescPersonSegment resultSegment = new DescPersonSegment(gen);
                resultSegment.IRec = iRec;
                fSegments.Add(resultSegment);

                if (gen < fVisibleGenerations) {
                    var tree = fBase.Context.Tree;
                    int numberOfFamilyLinks = iRec.SpouseToFamilyLinks.Count;
                    for (int j = 0; j < numberOfFamilyLinks; j++) {
                        GDMFamilyRecord family = tree.GetPtrValue(iRec.SpouseToFamilyLinks[j]);
                        if (!fBase.Context.IsRecordAccess(family.Restriction)) continue;

                        fBase.Context.ProcessFamily(family);

                        int numberOfChildren = family.Children.Count;
                        for (int i = 0; i < numberOfChildren; i++) {
                            GDMIndividualRecord child = tree.GetPtrValue(family.Children[i]);
                            DescPersonSegment childSegment = TraverseDescendants(child, gen + 1);

                            int size = Math.Max(1, childSegment.TotalSubSegments);
                            resultSegment.TotalSubSegments += size;

                            resultSegment.ChildSegments.Add(childSegment);
                        }
                    }
                }

                return resultSegment;
            } catch {
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

                DrawSegment(segment, fPen, brush);
            }
        }

        #endregion
    }
}
