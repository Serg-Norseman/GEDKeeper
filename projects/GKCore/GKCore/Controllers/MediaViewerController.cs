/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Media;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class MediaViewerController : DialogController<IMediaViewerWin>
    {
        private GDMFileReferenceWithTitle fFileReference;
        private GDMMultimediaRecord fMultimedia;

        public GDMMultimediaRecord MultimediaRecord
        {
            get { return fMultimedia; }
            set {
                fMultimedia = value;
                // FIXME
                /*GDMFileReferenceWithTitle fileRef = fMultimedia.FileReferences[0];
                if (fFileReference != fileRef) {
                    fFileReference = fileRef;
                    UpdateView();
                }*/
            }
        }

        public GDMFileReferenceWithTitle FileReference
        {
            get { return fFileReference; }
            set {
                fFileReference = value;
                UpdateView();
            }
        }

        public MediaViewerController(IMediaViewerWin view) : base(view)
        {
            
        }

        public override void UpdateView()
        {
            if (fFileReference == null) return;

            fView.SetTitle(fFileReference.Title);

            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fFileReference.GetMultimediaFormat());

            try {
                switch (mmKind) {
                    case MultimediaKind.mkImage:
                        {
                            int fileNum = fMultimedia.FileReferences.IndexOf(fFileReference);
                            IImage img = fBase.Context.LoadMediaImage(fMultimedia, fileNum, -1, -1, ExtRect.Empty, false, false);
                            if (img != null) {
                                fView.SetViewImage(img);
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
                            if (fs != null) {
                                bool disposeStream = true;
                                switch (fFileReference.GetMultimediaFormat()) {
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
                            }
                            break;
                        }
                }
            } catch (Exception ex) {
                fView.DisposeViewControl();
                Logger.WriteError("MediaViewerController.UpdateView()", ex);
            }
        }

        public void ProcessPortraits(IImageView imageCtl)
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
