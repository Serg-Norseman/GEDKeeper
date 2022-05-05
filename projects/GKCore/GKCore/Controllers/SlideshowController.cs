/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using BSLib.Design.Graphics;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class SlideshowController : FormController<ISlideshowWin>
    {
        private readonly List<GDMFileReferenceWithTitle> fFileRefs;
        private int fCurrentIndex;
        private string fCurrentText;

        public List<GDMFileReferenceWithTitle> FileRefs
        {
            get { return fFileRefs; }
        }

        public int CurrentIndex
        {
            get { return fCurrentIndex; }
        }

        public SlideshowController(ISlideshowWin view) : base(view)
        {
            fFileRefs = new List<GDMFileReferenceWithTitle>();
            fCurrentIndex = -1;
        }

        public void LoadList()
        {
            GDMRecord record;
            var enumerator = fBase.Context.Tree.GetEnumerator(GDMRecordType.rtMultimedia);
            while (enumerator.MoveNext(out record)) {
                GDMMultimediaRecord mediaRec = (GDMMultimediaRecord)record;
                GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];

                MultimediaKind mmKind = GKUtils.GetMultimediaKind(fileRef.MultimediaFormat);
                if (mmKind == MultimediaKind.mkImage) {
                    fFileRefs.Add(fileRef);
                }
            }
        }

        public override void UpdateView()
        {
            fView.StatusLines[0] = string.Format("{0} / {1} [{2}]", fCurrentIndex + 1, fFileRefs.Count, fCurrentText);
            fView.UpdateControls();
        }

        private void SetFileRef()
        {
            if (fCurrentIndex < 0 || fCurrentIndex >= fFileRefs.Count) return;

            // Only images are in the list
            GDMFileReferenceWithTitle fileRef = fFileRefs[fCurrentIndex];

            fCurrentText = fileRef.Title;

            IImage img = fBase.Context.LoadMediaImage(fileRef, false);
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

        public override void SetLocale()
        {
        }
    }
}
