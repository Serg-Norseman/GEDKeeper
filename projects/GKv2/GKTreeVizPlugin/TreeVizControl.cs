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
using System.Drawing;
using System.Drawing.Imaging;
using System.Timers;
using System.Windows.Forms;

using ArborGVT;
using BSLib;
using CsGL.OpenGL;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Components;

namespace GKTreeVizPlugin
{
    public sealed class TreeVizControl : OpenGLControl
    {
        private const float BASE_SCALE = 10.0f;
        private const bool EXCLUDE_CHILDLESS = true;	// exclude childless branches of the tree
        private const float MAGIC_SCALE = 4;			// scaling factor in the transition from the Arbor to the 3D
        private const float DEG2RAD = 3.14159F / 180;
        private const uint OBJ_X = 1;
        private const uint OBJ_Y = 2;
        private const uint OBJ_Z = 3;
        private const uint OBJ_NODE = 100;

        private const byte ACCUM_DEPTH = 0;         // OpenGL's Accumulation Buffer Depth, In Bits Per Pixel.
        private const byte STENCIL_DEPTH = 0;       // OpenGL's Stencil Buffer Depth, In Bits Per Pixel.
        private const byte Z_DEPTH = 16;            // OpenGL's Z-Buffer Depth, In Bits Per Pixel.
        private const byte COLOR_DEPTH = 16;        // The Current Color Depth, In Bits Per Pixel.
        private const double NEAR_CLIPPING_PLANE = 0.1f;    // GLU's Distance From The Viewer To The Near Clipping Plane (Always Positive).
        private const double FAR_CLIPPING_PLANE = 1000.0f;  // GLU's Distance From The Viewer To The Far Clipping Plane (Always Positive).
        private const double FOV_Y = 45.0f;         // GLU's Field Of View Angle, In Degrees, In The Y Direction.

        // unused
        //private float[] LightAmbient = {0.5f, 0.5f, 0.5f, 1.0f};
        //private float[] LightDiffuse = {1.0f, 1.0f, 1.0f, 1.0f};
        //private float[] LightPosition = {0.0f, 0.0f, 2.0f, 1.0f};
        //private int filter = 0;									// Which Filter To Use
        //private uint[] texture = new uint[3];						// Storage For 3 Textures

        // rendering
        private float xrot;
        private float yrot;
        private float zrot;
        private float z;

        // control
        private int fHeight;
        private int fWidth;
        private bool fMouseDrag;
        private int fLastX;
        private int fLastY;
        private bool fFreeRotate;

        // runtime
        private IBaseWindow fBase;
        private ArborSystem fSys;
        private readonly List<TVPerson> fPersons;
        private readonly Dictionary<string, TVPerson> fPersonsIndex;
        private readonly List<TVStem> fStems;
        private int fMaxYear;
        private int fMinYear;
        private float fYearSize;
        private bool fDebug;

        // animation
        private System.Timers.Timer fAnimTimer;
        private bool fBusy;
        private int fCurYear;
        private uint fTick;


        public int CurYear
        {
            get { return fCurYear; }
        }

        public bool Debug
        {
            get { return fDebug; }
            set { fDebug = value; }
        }

        public bool TimeStop { get; set; }
        public string SelectedObject { get; private set; }

        public bool FreeRotate
        {
            get {
                return fFreeRotate;
            }
            set {
                fFreeRotate = value;
                if (value) return;

                xrot = -75;
                yrot = 0.0F;
            }
        }


        public TreeVizControl()
        {
            xrot = 0;
            yrot = 0;
            zrot = 0;
            z = -5.0f;

            OpenGL.glShadeModel(OpenGL.GL_SMOOTH);
            //GL.glClearColor(0.0f, 0.0f, 0.0f, 0.5f);
            OpenGL.glClearColor(0.25f, 0.25f, 0.25f, 0.5f);
            OpenGL.glClearDepth(1.0f);
            OpenGL.glEnable(OpenGL.GL_DEPTH_TEST);
            OpenGL.glDepthFunc(OpenGL.GL_LEQUAL);
            OpenGL.glHint(OpenGL.GL_PERSPECTIVE_CORRECTION_HINT, OpenGL.GL_NICEST);

            //glEnable(GL_TEXTURE_2D);
            //LoadTextures();
            //GL.glLightfv(GL.GL_LIGHT1, GL.GL_AMBIENT, this.LightAmbient);
            //GL.glLightfv(GL.GL_LIGHT1, GL.GL_DIFFUSE, this.LightDiffuse);
            //GL.glLightfv(GL.GL_LIGHT1, GL.GL_POSITION, this.LightPosition);
            //GL.glEnable(GL.GL_LIGHT1);

            Context.Grab();
            OpenGLException.Assert();

            fPersons = new List<TVPerson>();
            fPersonsIndex = new Dictionary<string, TVPerson>();
            fStems = new List<TVStem>();

            fDebug = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fSys != null) fSys.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Control routines

        private uint retrieveObjectId(int x, int y)
        {
            /*int[] viewportCoords = {0, 0, 0, 0};
			uint[] selectBuffer = new uint[32];

			GL.glSelectBuffer(32, selectBuffer);
			GL.glGetIntegerv(GL.GL_VIEWPORT, viewportCoords);
			GL.glMatrixMode(GL.GL_PROJECTION);
			GL.glPushMatrix();
			GL.glRenderMode(GL.GL_SELECT);
			GL.glLoadIdentity();
			GL.gluPickMatrix(x, viewportCoords[3] - y, 2, 2, viewportCoords);
			GL.gluPerspective(FOV_Y, (float) fWidth / fHeight, NEAR_CLIPPING_PLANE, FAR_CLIPPING_PLANE);
			GL.glMatrixMode(GL.GL_MODELVIEW);

			this.glDraw();

            int objectsFound = GL.glRenderMode(GL.GL_RENDER);

			GL.glMatrixMode(GL.GL_PROJECTION);
			GL.glPopMatrix();
			GL.glMatrixMode(GL.GL_MODELVIEW);

			if (objectsFound > 0)
			{
				uint lowestDepth = selectBuffer[1];
				uint selectedObject = selectBuffer[3];

				for (int i = 1; i < objectsFound; i++)
				{
					if (selectBuffer[(i * 4) + 1] < lowestDepth)
					{
						lowestDepth = selectBuffer[(i * 4) + 1];
						selectedObject = selectBuffer[(i * 4) + 3];
					}
				}

				return selectedObject;
			}*/

            return 0;
        }

        protected override bool IsInputKey(Keys keyData)
        {
            switch(keyData) {
                case Keys.Up:
                case Keys.Down:
                case Keys.Right:
                case Keys.Left:
                case Keys.Tab:
                    return true;
                default:
                    return base.IsInputKey(keyData);
            }
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.KeyCode) {
                case Keys.F5:
                    Screenshot();
                    break;

                case Keys.PageDown:
                    z -= 0.5f;
                    break;

                case Keys.PageUp:
                    z += 0.5f;
                    break;

                case Keys.R:
                    /*if (e.Control)*/ FreeRotate = !FreeRotate;
                    break;

                case Keys.D:
                    /*if (e.Control)*/ fDebug = !fDebug;
                    break;

                case Keys.T:
                    TimeStop = !TimeStop;
                    break;

                default:
                    base.OnKeyDown(e);
                    break;
            }
        }

        protected override void OnKeyUp(KeyEventArgs e)
        {
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (!Focused) Focus();

            if (e.Button == MouseButtons.Left) {
                fMouseDrag = true;
                fLastX = e.X;
                fLastY = e.Y;
            }
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left) {
                fMouseDrag = false;
            }

            base.OnMouseUp(e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            if (fMouseDrag) {
                int dx = e.X - fLastX;
                int dy = e.Y - fLastY;

                if (fFreeRotate) {
                    //xrot += 0.001f * dy;
                    //yrot += 0.001f * dx;

                    xrot += 0.001f * dy; //ok
                    zrot += 0.001f * dx; //ok
                } else {
                    zrot += 0.001f * dx;
                }
            } else {
                uint objectId = retrieveObjectId(e.X, e.Y);

                switch (objectId) {
                    case OBJ_X:
                        SelectedObject = "X-axis";
                        break;
                    case OBJ_Y:
                        SelectedObject = "Y-axis";
                        break;
                    case OBJ_Z:
                        SelectedObject = "Z-axis";
                        break;
                    default:
                        if (objectId >= OBJ_NODE) {
                            int id = (int)(objectId - OBJ_NODE);

                            TVPerson prs = FindPersonByIdx(id);
                            if (prs != null) {
                                SelectedObject = "["+prs.IRec.XRef+"] " + prs.IRec.GetPrimaryFullName()+
                                    ", " + prs.BirthYear.ToString() + " - " + prs.DeathYear.ToString();
                            } else {
                                SelectedObject = "<none>";
                            }
                        } else {
                            SelectedObject = "<none>";
                        }
                        break;
                }
            }
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            if (e.Delta != 0) {
                z += 0.01f * e.Delta;
            } else {
                base.OnMouseWheel(e);
            }
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            Size sz = Size;

            if (sz.Width != 0 && sz.Height != 0)
            {
                fHeight = sz.Height;
                fWidth = sz.Width;

                if (fHeight == 0) {
                    fHeight = 1;
                }

                OpenGL.glViewport(0, 0, fWidth, fHeight);
                OpenGL.glMatrixMode(OpenGL.GL_PROJECTION);
                OpenGL.glLoadIdentity();

                GLU.gluPerspective(FOV_Y, (float)fWidth / fHeight, NEAR_CLIPPING_PLANE, FAR_CLIPPING_PLANE);

                OpenGL.glMatrixMode(OpenGL.GL_MODELVIEW);
                OpenGL.glLoadIdentity();
            }

            base.OnSizeChanged(e);
        }

        protected override OpenGLContext CreateContext()
        {
            ControlGLContext context = new ControlGLContext(this);
            DisplayType displayType = new DisplayType(COLOR_DEPTH, Z_DEPTH, STENCIL_DEPTH, ACCUM_DEPTH);
            context.Create(displayType, null);
            return context;
        }

        #endregion

        #region TreeViz

        public override void glDraw()
        {
            try
            {
                OpenGL.glClear(OpenGL.GL_COLOR_BUFFER_BIT | OpenGL.GL_DEPTH_BUFFER_BIT);
                OpenGL.glLoadIdentity();

                OpenGL.glInitNames();

                OpenGL.glTranslatef(0.0f, 0.0f, z);
                OpenGL.glRotatef(xrot, 1.0f, 0.0f, 0.0f);
                OpenGL.glRotatef(yrot, 0.0f, 1.0f, 0.0f);
                OpenGL.glRotatef(zrot, 0.0f, 0.0f, 1.0f);

                DrawAxis();

                // FIXME: labels of centuries are displayed from the beginning a personalized timeline,
                // need to alter the output according to chronological ages
                for (int i = 0; i <= 5; i++) {
                    OpenGL.glPushMatrix();
                    OpenGL.glTranslatef(0, 0, i * 100 * fYearSize);
                    OpenGL.glColor3f(0.9F, 0.1F, 0.1F);
                    DrawCircle(0.1F);
                    OpenGL.glPopMatrix();
                }

                DrawArborSystem();

                int num = fPersons.Count;
                for (int i = 0; i < num; i++)
                {
                    TVPerson prs = fPersons[i];
                    DrawPerson(prs);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.glDraw(): " + ex.Message);
            }
        }

        public void CreateArborGraph(IBaseWindow baseWin, int minGens, bool loneSuppress)
        {
            fBase = baseWin;

            try
            {
                fSys = new ArborSystemEx(1000, 1000, 0.1, null); //(10000, 1000, 0.1, this);
                fSys.setScreenSize(50, 50);
                fSys.OnStop += OnArborStop;

                using (ExtList<PatriarchObj> patList = PatriarchsMan.GetPatriarchsLinks(
                    baseWin.Context, minGens, false, loneSuppress))
                {
                    int num = patList.Count;
                    for (int i = 0; i < num; i++) {
                        PatriarchObj pObj = patList[i];

                        if (!loneSuppress || pObj.HasLinks) {
                            ArborNode node = fSys.addNode(pObj.IRec.XRef);
                            node.Data = pObj;
                        }
                    }

                    for (int i = 0; i < num; i++)
                    {
                        PatriarchObj pat1 = patList[i];

                        foreach (PatriarchObj pat2 in pat1.Links)
                        {
                            fSys.addEdge(pat1.IRec.XRef, pat2.IRec.XRef);
                        }
                    }
                }

                z = -50;

                fSys.start();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.CreateArborGraph(): " + ex.Message);
            }
        }

        public void OnArborStop(object sender, EventArgs eArgs)
        {
            FreeRotate = false;

            fMinYear = 0;

            try
            {
                // load from ArborSystem points and signatures of the patriarchs
                foreach (ArborNode node in fSys.Nodes)
                {
                    PatriarchObj patObj = (PatriarchObj)node.Data;

                    GDMIndividualRecord iRec = (GDMIndividualRecord)fBase.Context.Tree.XRefIndex_Find(node.Sign);
                    int descGens = patObj.DescGenerations;

                    TVPerson patr = PreparePerson(null, iRec, TVPersonType.Patriarch);
                    if (patr != null) {
                        patr.Pt = new PointF((float)node.Pt.X * MAGIC_SCALE, (float)node.Pt.Y * MAGIC_SCALE);
                        patr.DescGenerations = descGens;
                        patr.BaseRadius = 100;

                        ProcessPersonStem(patr, null, TVPersonType.Patriarch);

                        if (fMinYear == 0) {
                            fMinYear = patr.BirthYear;
                        } else {
                            if (fMinYear > patr.BirthYear) fMinYear = patr.BirthYear;
                        }
                    }
                }

                // prepare the radii of the bases of the patriarchs
                foreach (ArborEdge edge in fSys.Edges)
                {
                    TVPerson srcPers = FindPersonByXRef(edge.Source.Sign);
                    TVPerson tgtPers = FindPersonByXRef(edge.Target.Sign);
                    if (srcPers == null || tgtPers == null) continue;

                    float rad = (float)Dist(srcPers.Pt, tgtPers.Pt) * 3/7;

                    if (srcPers.BaseRadius > rad) srcPers.BaseRadius = rad;
                    if (tgtPers.BaseRadius > rad) tgtPers.BaseRadius = rad;
                }

                // prepare the range of years
                fMaxYear = DateTime.Now.Year;
                fYearSize = BASE_SCALE / (fMaxYear - fMinYear);
                fTick = 0;
                fCurYear = fMinYear;

                // prepare tree, the base number - only the patriarchs
                int count = fPersons.Count;
                for (int i = 0; i < count; i++)
                {
                    TVPerson prs = fPersons[i];
                    PrepareDescendants(prs);
                }

                for (int i = 0; i < fStems.Count; i++)
                {
                    TVStem stem = fStems[i];
                    stem.Update();
                }

                StartTimer();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.onArborStop(): " + ex.Message);
            }
        }

        private void PrepareDescendants(TVPerson person)
        {
            if (person == null) return;

            try
            {
                int gens = (person.DescGenerations <= 0) ? 1 : person.DescGenerations;
                person.GenSlice = person.BaseRadius / gens; // ?

                GDMIndividualRecord iRec = person.IRec;

                foreach (GDMSpouseToFamilyLink spLink in iRec.SpouseToFamilyLinks)
                {
                    GDMFamilyRecord famRec = spLink.Family;

                    bool alreadyPrepared = false;

                    // processing the spouse of the current person
                    GDMIndividualRecord spouse = famRec.GetSpouseBy(iRec);
                    if (spouse != null) {
                        TVPerson sps = PreparePerson(null, spouse, TVPersonType.Spouse);
                        if (sps == null) {
                            // this can occur only when processing of the patriarchs later than those already processed,
                            // i.e. if the at first was already processed the patriarch, born in 1710, was processed his childrens
                            // and one of theirs spouses being a patriarch of new branches!
                            Logger.LogWrite("TreeVizControl.PrepareDescendants(): an unexpected collision");
                            alreadyPrepared = true;
                        } else {
                            ProcessPersonStem(sps, person, TVPersonType.Spouse);

                            person.Spouses.Add(sps);
                        }
                    }

                    if (!alreadyPrepared)
                    {
                        // processing children of the current family
                        foreach (GDMPointer childPtr in famRec.Children)
                        {
                            GDMIndividualRecord child = (GDMIndividualRecord)childPtr.Value;

                            // exclude childless branches
                            if (EXCLUDE_CHILDLESS && (fBase.Context.IsChildless(child) || child.GetTotalChildsCount() < 1)) continue;

                            TVPerson chp = PreparePerson(person, child, TVPersonType.Child);
                            if (chp == null) {
                                // this is someone spouse and already prepared, intersection of branches
                                Logger.LogWrite("TreeVizControl.PrepareDescendants(): intersection");
                            } else {
                                chp.BaseRadius = (float)((person.BaseRadius / 2) * 0.95);
                                chp.DescGenerations = person.DescGenerations - 1;

                                ProcessPersonStem(chp, person, TVPersonType.Child);

                                person.Childs.Add(chp);

                                PrepareDescendants(chp);
                            }
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.PrepareDescendants(): " + ex.Message);
            }
        }

        private void RecalcDescendants(TVPerson person)
        {
            try
            {
                // recalculate the coordinates of the spouses, because the coordinates of this person could change
                // "genSlice / 3" - it's radius of spouses
                PointF[] pts = GetCirclePoints(person.BeautySpouses, person.Pt, person.Spouses.Count, person.GenSlice / 3);
                for (int i = 0, count = person.Spouses.Count; i < count; i++)
                {
                    TVPerson spp = person.Spouses[i];
                    if (IsVisible(spp)) {
                        spp.IsVisible = true;
                        person.Spouses[i].Pt = pts[i];
                    }
                }

                // recalculate of coordinates of visible children
                pts = GetCirclePoints(person.BeautyChilds, person.Pt, person.Childs.Count, person.BaseRadius / 2);
                for (int i = 0, count = person.Childs.Count; i < count; i++)
                {
                    TVPerson chp = person.Childs[i];

                    if (!chp.IsVisible && IsVisible(chp)) {
                        chp.IsVisible = true;
                    }

                    if (chp.IsVisible)
                    {
                        chp.Pt = pts[i];
                        chp.BaseRadius = (float)((person.BaseRadius / 2) * 0.95);
                        chp.DescGenerations = person.DescGenerations - 1;

                        RecalcDescendants(chp);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.recalcDescendants.2(): " + ex.Message);
            }
        }

        private void RecalcDescendants()
        {
            try
            {
                foreach (TVPerson prs in fPersons)
                {
                    if (prs.Type != TVPersonType.Patriarch || !IsVisible(prs)) continue;

                    prs.IsVisible = true;
                    RecalcDescendants(prs);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.recalcDescendants.1(): " + ex.Message);
            }
        }

        private void ProcessPersonStem(TVPerson person, TVPerson relative, TVPersonType type)
        {
            try
            {
                if (person == null) return;

                if (person.Stem == null && (type == TVPersonType.Patriarch || type == TVPersonType.Child))
                {
                    person.Stem = new TVStem();
                    fStems.Add(person.Stem);
                }

                switch (type)
                {
                    case TVPersonType.Spouse:
                        relative.Stem.AddSpouse(person);
                        break;

                    case TVPersonType.Child:
                        relative.Stem.AddChild(person);
                        break;

                    case TVPersonType.Patriarch:
                        break;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.ProcessPersonStem(): " + ex.Message);
            }
        }

        private TVPerson PreparePerson(TVPerson parent, GDMIndividualRecord iRec, TVPersonType type)
        {
            try
            {
                TVPerson result;

                if (fPersonsIndex.TryGetValue(iRec.XRef, out result))
                {
                    // the person is already in the general index; so this is someone's spouse in another tree previously processed

                    if (parent == null)
                    {
                        // parent == null, if this is patriarch or someone's spouse without his parents
                        // if it's a spouse and is already in the index - therefore,
                        // this person was married with the delegates of two different genera that are processed through their patriarchs?
                    }
                    else
                    {
                        result.Parent = parent;
                        PointF prevPt = result.Stem.Pt;
                        PointF parentPt = parent.Pt;
                        PointF midpoint = GetLineMidpoint(prevPt.X, prevPt.Y, parentPt.X, parentPt.Y);
                        result.Stem.Pt = midpoint;
                        result.Stem.Update();
                    }

                    return null;
                }
                else
                {
                    result = new TVPerson(parent, iRec);
                    result.Type = type;

                    fPersons.Add(result);
                    fPersonsIndex.Add(iRec.XRef, result);

                    result.BirthYear = fBase.Context.FindBirthYear(iRec);
                    result.DeathYear = fBase.Context.FindDeathYear(iRec);

                    // FIXME: alter to the pre-statistically a certain life expectancy
                    if (result.DeathYear == 0) result.DeathYear = result.BirthYear + 75;
                }

                return result;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.PreparePerson(): " + ex.Message);
                return null;
            }
        }

        private void DrawArborSystem()
        {
            if (fSys == null) return;
            if (!fDebug) return;

            try
            {
                foreach (ArborNode node in fSys.Nodes)
                {
                    ArborPoint pt = node.Pt;

                    OpenGL.glPushMatrix();
                    OpenGL.glTranslatef((float)pt.X * MAGIC_SCALE, (float)pt.Y * MAGIC_SCALE, 0);
                    OpenGL.glColor3f(0.9F, 0.3F, 0.3F);
                    DrawCircle(0.1F);
                    OpenGL.glPopMatrix();
                }

                foreach (ArborEdge edge in fSys.Edges)
                {
                    ArborPoint pt1 = edge.Source.Pt;
                    ArborPoint pt2 = edge.Target.Pt;

                    OpenGL.glPushMatrix();
                    OpenGL.glColor3f(0.9F, 0.3F, 0.3F);
                    OpenGL.glBegin(OpenGL.GL_LINES);
                    OpenGL.glVertex3f((float)pt1.X * MAGIC_SCALE, (float)pt1.Y * MAGIC_SCALE, 0);
                    OpenGL.glVertex3f((float)pt2.X * MAGIC_SCALE, (float)pt2.Y * MAGIC_SCALE, 0);
                    OpenGL.glEnd();
                    OpenGL.glPopMatrix();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.DrawArborSystem(): " + ex.Message);
            }
        }

        private bool IsVisible(TVPerson person)
        {
            if (person == null) return false;
            if (person.BirthYear > fCurYear) return false;

            // not to display a person for which auto-detection of dates to no avail
            return (person.BirthYear >= fMinYear && person.DeathYear >= fMinYear);
        }

        private void DrawPerson(TVPerson person)
        {
            if (person == null || !person.IsVisible) return;

            try
            {
                int endYear = (fCurYear < person.DeathYear) ? fCurYear : person.DeathYear;

                float zBirth, zDeath;
                zBirth = fYearSize * (person.BirthYear - fMinYear);
                zDeath = fYearSize * (endYear - fMinYear);

                PointF ppt = person.Pt;

                OpenGL.glPushName(OBJ_NODE + (uint)person.Idx);
                OpenGL.glPushMatrix();

                SetLineColor(person.Sex);

                OpenGL.glBegin(OpenGL.GL_LINES);
                OpenGL.glVertex3f(ppt.X, ppt.Y, zBirth);
                OpenGL.glVertex3f(ppt.X, ppt.Y, zDeath);
                OpenGL.glEnd();

                OpenGL.glPopMatrix();
                OpenGL.glPopName();

                if (fDebug && person.Type == TVPersonType.Patriarch) {
                    OpenGL.glPushMatrix();
                    OpenGL.glTranslatef(ppt.X, ppt.Y, 0);
                    OpenGL.glColor3f(0.9F, 0.1F, 0.1F);
                    DrawCircle(person.BaseRadius);
                    OpenGL.glPopMatrix();
                }

                if (person.Parent != null) {
                    PointF parentPt = person.Parent.Pt;

                    OpenGL.glPushMatrix();

                    SetLineColor(person.Sex);

                    OpenGL.glBegin(OpenGL.GL_LINES);
                    OpenGL.glVertex3f(parentPt.X, parentPt.Y, zBirth);
                    OpenGL.glVertex3f(ppt.X, ppt.Y, zBirth);
                    OpenGL.glEnd();

                    OpenGL.glPopMatrix();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.DrawPerson(): " + ex.Message);
            }
        }

        private void UpdateTV(object sender, ElapsedEventArgs e)
        {
            if (fBusy) return;
            fBusy = true;
            try
            {
                if (!FreeRotate) {
                    zrot -= 0.3f;
                }

                fTick += 1;

                if (!TimeStop && (fTick % 5 == 0) && fCurYear < fMaxYear)
                {
                    fCurYear += 1;

                    RecalcDescendants();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.UpdateTV(): " + ex.Message);
            }
            fBusy = false;
        }

        #region Utils

        private TVPerson FindPersonByXRef(string xref)
        {
            TVPerson result;
            return fPersonsIndex.TryGetValue(xref, out result) ? result : null;
        }

        private TVPerson FindPersonByIdx(int idx)
        {
            foreach (TVPerson prs in fPersons)
            {
                if (prs.Idx == idx) return prs;
            }

            return null;
        }

        private void Screenshot()
        {
            try {
                using (Image image = Context.ToImage()) {
                    image.Save(@"d:\Screenshot.jpg", ImageFormat.Jpeg);
                }
            }
            catch (Exception e) {
                MessageBox.Show(e.Message, @"Error", MessageBoxButtons.OK, MessageBoxIcon.Stop);
            }
        }

        #endregion

        #region Low-level draw methods

        private static PointF GetLineMidpoint(float x1, float y1, float x2, float y2)
        {
            float mx = x1 + (x2 - x1) / 2;
            float my = y1 + (y2 - y1) / 2;
            return new PointF(mx, my);
        }

        // beauty - random offset for "beauty", grad
        internal static PointF[] GetCirclePoints(int beauty, PointF center, int count, float radius)
        {
            PointF[] result = new PointF[count];

            if (count > 0)
            {
                // size of the circle's partition, grad
                float degSection = 360.0f / count;

                for (int i = 0; i < count; i++)
                {
                    float degInRad = (beauty + i * degSection) * DEG2RAD;
                    float dx = (float)Math.Cos(degInRad) * radius;
                    float dy = (float)Math.Sin(degInRad) * radius;

                    result[i] = new PointF(center.X + dx, center.Y + dy);
                }
            }

            return result;
        }

        private static double Dist(PointF pt1, PointF pt2)
        {
            double dx = pt2.X - pt1.X;
            double dy = pt2.Y - pt1.Y;
            return Math.Sqrt(dx * dx + dy * dy);
        }

        private static void SetLineColor(GEDCOMSex sex)
        {
            switch (sex) {
                case GEDCOMSex.svMale:
                    OpenGL.glColor3f(0.1F, 0.3F, 0.9F);
                    break;

                case GEDCOMSex.svFemale:
                    OpenGL.glColor3f(0.9F, 0.3F, 0.1F);
                    break;
            }
        }

        private static void DrawCircle(float radius)
        {
            OpenGL.glBegin(OpenGL.GL_LINE_LOOP);
            for (int i = 0; i <= 360; i++) {
                float degInRad = i * DEG2RAD;
                OpenGL.glVertex2f((float)Math.Cos(degInRad) * radius, (float)Math.Sin(degInRad) * radius);
            }
            OpenGL.glEnd();
        }

        private static void DrawAxis()
        {
            // draw z-axis
            OpenGL.glPushName(OBJ_Z);
            OpenGL.glColor3f(1.0F, 1.0F, 1.0F);
            OpenGL.glBegin(OpenGL.GL_LINES); // z
            OpenGL.glVertex3f(0, 0, 0);
            OpenGL.glVertex3f(0, 0, 100);
            OpenGL.glEnd();
            OpenGL.glColor3f(0.5F, 0.5F, 0.5F);
            OpenGL.glBegin(OpenGL.GL_LINES); // z
            OpenGL.glVertex3f(0, 0, 0);
            OpenGL.glVertex3f(0, 0, -100);
            OpenGL.glEnd();
            OpenGL.glPopName();

            // draw y-axis
            OpenGL.glPushName(OBJ_Y);
            OpenGL.glColor3f(0.0F, 0.0F, 1.0F);
            OpenGL.glBegin(OpenGL.GL_LINES); // y
            OpenGL.glVertex3f(0, 0, 0);
            OpenGL.glVertex3f(0, 100, 0);
            OpenGL.glEnd();
            OpenGL.glColor3f(0.0F, 0.0F, 0.5F);
            OpenGL.glBegin(OpenGL.GL_LINES); // y
            OpenGL.glVertex3f(0, 0, 0);
            OpenGL.glVertex3f(0, -100, 0);
            OpenGL.glEnd();
            OpenGL.glPopName();

            // draw x-axis
            OpenGL.glPushName(OBJ_X);
            OpenGL.glColor3f(0.0F, 1.0F, 0.0F);
            OpenGL.glBegin(OpenGL.GL_LINES); // x
            OpenGL.glVertex3f(0, 0, 0);
            OpenGL.glVertex3f(100, 0, 0);
            OpenGL.glEnd();
            OpenGL.glColor3f(0.0F, 0.5F, 0.0F);
            OpenGL.glBegin(OpenGL.GL_LINES); // x
            OpenGL.glVertex3f(0, 0, 0);
            OpenGL.glVertex3f(-100, 0, 0);
            OpenGL.glEnd();
            OpenGL.glPopName();
        }

        #endregion

        #region Timer control

        private void StartTimer()
        {
            if (fAnimTimer != null) return;

            fAnimTimer = new System.Timers.Timer();
            fAnimTimer.AutoReset = true;
            fAnimTimer.Interval = 20; //50;
            fAnimTimer.Elapsed += UpdateTV;
            fAnimTimer.Start();
        }

        private void StopTimer()
        {
            if (fAnimTimer == null) return;

            fAnimTimer.Stop();
            fAnimTimer = null;
        }

        #endregion

        #endregion
    }
}
