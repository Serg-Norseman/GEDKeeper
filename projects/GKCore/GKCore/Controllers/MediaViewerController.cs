/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.IO;
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
        private GDMFileReferenceWithTitle fFileRef;

        public GDMFileReferenceWithTitle FileRef
        {
            get { return fFileRef; }
            set {
                if (fFileRef != value) {
                    fFileRef = value;
                    UpdateView();
                }
            }
        }

        public MediaViewerController(IMediaViewerWin view) : base(view)
        {
            
        }

        public override void UpdateView()
        {
            fView.Title = fFileRef.Title;

            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fFileRef.MultimediaFormat);

            try {
                switch (mmKind) {
                    case MultimediaKind.mkImage:
                        {
                            IImage img = fBase.Context.LoadMediaImage(fFileRef, false);
                            if (img != null) {
                                fView.SetViewImage(img, fFileRef);
                            }
                            break;
                        }

                    case MultimediaKind.mkAudio:
                    case MultimediaKind.mkVideo:
                        {
                            string targetFile = fBase.Context.MediaLoad(fFileRef);
                            fView.SetViewMedia(targetFile);
                            break;
                        }

                    case MultimediaKind.mkText:
                        {
                            Stream fs = fBase.Context.MediaLoad(fFileRef, false);
                            bool disposeStream = (fs != null);

                            switch (fFileRef.MultimediaFormat) {
                                case GDMMultimediaFormat.mfTXT:
                                    using (StreamReader strd = new StreamReader(fs)) {
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
                Logger.LogWrite("MediaViewerController.UpdateView(): " + ex.Message);
            }
        }

        public void ProcessPortraits(IImageView imageCtl, GDMFileReferenceWithTitle fileRef)
        {
            var mmRec = fileRef.Owner as GDMMultimediaRecord;

            var linksList = new List<GDMObject>();
            GKUtils.SearchRecordLinks(linksList, mmRec.GetTree(), mmRec);

            bool showRegions = false;
            foreach (var link in linksList) {
                var mmLink = link as GDMMultimediaLink;
                if (mmLink != null && mmLink.IsPrimary) {
                    var indiRec = mmLink.Owner as GDMIndividualRecord;
                    string indiName = GKUtils.GetNameString(indiRec, true, false);
                    var region = mmLink.CutoutPosition.Value;

                    imageCtl.AddNamedRegion(indiName, region);
                    showRegions = true;
                }
            }

            imageCtl.ShowNamedRegionTips = showRegions;
        }
    }
}
