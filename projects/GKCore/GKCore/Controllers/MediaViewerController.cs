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

using System;
using System.IO;
using BSLib;
using BSLib.Design.Graphics;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class MediaViewerController : DialogController<IMediaViewerWin>
    {
        private GDMFileReferenceWithTitle fFileReference;
        private GDMMultimediaRecord fMultimedia;

        public GDMFileReferenceWithTitle FileReference
        {
            get { return fFileReference; }
            set {
                if (fFileReference != value) {
                    fFileReference = value;
                    UpdateView();
                }
            }
        }

        public GDMMultimediaRecord MultimediaRecord
        {
            get { return fMultimedia; }
            set { fMultimedia = value; }
        }

        public MediaViewerController(IMediaViewerWin view) : base(view)
        {
            
        }

        public override void UpdateView()
        {
            fView.Title = fFileReference.Title;

            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fFileReference.MultimediaFormat);

            try {
                switch (mmKind) {
                    case MultimediaKind.mkImage:
                        {
                            IImage img = fBase.Context.LoadMediaImage(fFileReference, false);
                            if (img != null) {
                                fView.SetViewImage(img, fFileReference);
                            }
                            break;
                        }

                    case MultimediaKind.mkAudio:
                    case MultimediaKind.mkVideo:
                        {
                            string targetFile = fBase.Context.MediaLoad(fFileReference);
                            fView.SetViewMedia(targetFile);
                            break;
                        }

                    case MultimediaKind.mkText:
                        {
                            Stream fs = fBase.Context.MediaLoad(fFileReference, false);
                            bool disposeStream = (fs != null);

                            switch (fFileReference.MultimediaFormat) {
                                case GDMMultimediaFormat.mfTXT:
                                    using (StreamReader strd = GKUtils.GetDetectedStreamReader(fs)) {
                                        string text = strd.ReadToEnd();
                                        fView.SetViewText(text);
                                    }
                                    break;

                                case GDMMultimediaFormat.mfRTF:
                                    using (StreamReader strd = new StreamReader(fs)) {
                                        string text = strd.ReadToEnd();
                                        fView.SetViewRTF(text);
                                    }
                                    break;

                                case GDMMultimediaFormat.mfHTM:
                                    disposeStream = false;
                                    fView.SetViewHTML(fs);
                                    break;
                            }
                            if (disposeStream) fs.Dispose();
                            break;
                        }
                }
            } catch (Exception ex) {
                fView.DisposeViewControl();
                Logger.WriteError("MediaViewerController.UpdateView()", ex);
            }
        }

        public void ProcessPortraits(IImageView imageCtl, GDMFileReferenceWithTitle fileRef)
        {
            var portraits = GKUtils.SearchPortraits(fBase.Context.Tree, fMultimedia);

            bool showRegions = (portraits.Count > 0);
            if (showRegions) {
                for (int i = 0; i < portraits.Count; i++) {
                    imageCtl.AddNamedRegion(portraits[i], (ExtRect)portraits.GetObject(i));
                }
            }
            imageCtl.ShowNamedRegionTips = showRegions;
        }

        public override void SetLocale()
        {
        }
    }
}
