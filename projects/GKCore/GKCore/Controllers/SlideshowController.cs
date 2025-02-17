/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class SlideshowController : FormController<ISlideshowWin>
    {
        private sealed class ImageRef
        {
            public GDMMultimediaRecord MediaRec;
            public string Title;

            public ImageRef(GDMMultimediaRecord mediaRec, string title)
            {
                MediaRec = mediaRec;
                Title = title;
            }
        }

        private readonly List<ImageRef> fFileRefs;
        private bool fActive;
        private int fCurrentIndex;
        private string fCurrentText;
        private ITimer fTimer;

        /*public List<ImageRef> FileRefs
        {
            get { return fFileRefs; }
        }

        public int CurrentIndex
        {
            get { return fCurrentIndex; }
        }*/

        public SlideshowController(ISlideshowWin view) : base(view)
        {
            fFileRefs = new List<ImageRef>();
            fCurrentIndex = -1;
            fTimer = AppHost.Instance.CreateTimer(1000, Timer1Tick);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fTimer != null)
                    fTimer.Dispose();
            }
            base.Dispose(disposing);
        }

        private void Timer1Tick(object sender, EventArgs e)
        {
            Next();
        }

        public void LoadList()
        {
            GDMMultimediaRecord mediaRec;
            var enumerator = fBase.Context.Tree.GetEnumerator<GDMMultimediaRecord>();
            while (enumerator.MoveNext(out mediaRec)) {
                GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];

                MultimediaKind mmKind = GKUtils.GetMultimediaKind(fileRef.GetMultimediaFormat());
                if (mmKind == MultimediaKind.mkImage) {
                    fFileRefs.Add(new ImageRef(mediaRec, fileRef.Title));
                }
            }
        }

        public override void UpdateView()
        {
            fView.StatusLines[0] = string.Format("{0} / {1} [{2}]", fCurrentIndex + 1, fFileRefs.Count, fCurrentText);

            GetControl<IButtonToolItem>("tbStart").Enabled = (fFileRefs.Count > 0);
            GetControl<IButtonToolItem>("tbPrev").Enabled = (fCurrentIndex > 0);
            GetControl<IButtonToolItem>("tbNext").Enabled = (fCurrentIndex < fFileRefs.Count - 1);
        }

        private void SetFileRef()
        {
            if (fCurrentIndex < 0 || fCurrentIndex >= fFileRefs.Count) return;

            // Only images are in the list
            var imageRef = fFileRefs[fCurrentIndex];

            fCurrentText = imageRef.Title;

            IImage img = fBase.Context.LoadMediaImage(imageRef.MediaRec, -1, -1, ExtRect.Empty, false);
            if (img != null) {
                fView.SetImage(img);
            }

            UpdateView();
        }

        public void Prev()
        {
            if (fCurrentIndex == 0) {
                fCurrentIndex = fFileRefs.Count - 1;
            } else {
                fCurrentIndex--;
            }

            SetFileRef();
        }

        public void Next()
        {
            if (fCurrentIndex < fFileRefs.Count - 1) {
                fCurrentIndex++;
            } else {
                fCurrentIndex = 0;
            }

            SetFileRef();
        }

        public bool SwitchActive()
        {
            if (!fActive) {
                GetControl<IButtonToolItem>("tbStart").Text = LangMan.LS(LSID.Stop);
                fTimer.Start();
            } else {
                GetControl<IButtonToolItem>("tbStart").Text = LangMan.LS(LSID.Start);
                fTimer.Stop();
            }

            fActive = !fActive;

            return fActive;
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.Slideshow);

            GetControl<IButtonToolItem>("tbStart").Text = LangMan.LS(LSID.Start);

            SetToolTip("tbPrev", LangMan.LS(LSID.PrevRec));
            SetToolTip("tbNext", LangMan.LS(LSID.NextRec));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IToolItem>("tbStart").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Start, true);
            GetControl<IToolItem>("tbPrev").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Prev, true);
            GetControl<IToolItem>("tbNext").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Next, true);
        }
    }
}
