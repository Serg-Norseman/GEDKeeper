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
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Media;
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
        private readonly ITimer fTimer;

        private bool fActive;
        private int fCurrentIndex;
        private string fCurrentText;

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

            GetControl<IToolItem>("tbStart").Enabled = (fFileRefs.Count > 0);
            GetControl<IToolItem>("tbPrev").Enabled = (fCurrentIndex > 0);
            GetControl<IToolItem>("tbNext").Enabled = (fCurrentIndex < fFileRefs.Count - 1);
        }

        private void SetFileRef()
        {
            if (fCurrentIndex < 0 || fCurrentIndex >= fFileRefs.Count) return;

            // Only images are in the list
            var imageRef = fFileRefs[fCurrentIndex];

            fCurrentText = imageRef.Title;

            IImage img = fBase.Context.LoadMediaImage(imageRef.MediaRec, 0, -1, -1, ExtRect.Empty, false, false);
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
                GetControl<IToolItem>("tbStart").Text = LangMan.LS(LSID.Stop);
                GetControl<IToolItem>("tbStart").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Stop, true);
                fTimer.Start();
            } else {
                GetControl<IToolItem>("tbStart").Text = LangMan.LS(LSID.Start);
                GetControl<IToolItem>("tbStart").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Start, true);
                fTimer.Stop();
            }

            fActive = !fActive;

            return fActive;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Slideshow));

            GetControl<IToolItem>("tbStart").Text = LangMan.LS(LSID.Start);

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
