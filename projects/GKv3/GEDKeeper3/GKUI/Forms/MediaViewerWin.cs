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
using System.ComponentModel;
using System.IO;
using System.Text;
using Eto.Drawing;
using Eto.Forms;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public partial class MediaViewerWin : Form, ILocalization
    {
        private readonly IBaseWindow fBase;
        private GEDCOMFileReferenceWithTitle fFileRef;
        private ITimer fTimer;
        private Control fViewer;

        public GEDCOMFileReferenceWithTitle FileRef
        {
            get { return fFileRef; }
            set { SetFileRef(value); }
        }

        private void SetFileRef(GEDCOMFileReferenceWithTitle value)
        {
            fFileRef = value;
            Title = fFileRef.Title;
            Control ctl = null;

            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fFileRef.MultimediaFormat);

            try {
                switch (mmKind) {
                    case MultimediaKind.mkImage:
                        {
                            IImage img = fBase.Context.LoadMediaImage(fFileRef, false);
                            if (img != null) {
                                SetViewImage(((ImageHandler)img).Handle, fFileRef);
                            }
                            break;
                        }

                    case MultimediaKind.mkAudio:
                    case MultimediaKind.mkVideo:
                        {
                            string targetFile = fBase.Context.MediaLoad(fFileRef);
                            SetViewMedia(targetFile);
                            break;
                        }

                    case MultimediaKind.mkText:
                        {
                            Stream fs = fBase.Context.MediaLoad(fFileRef, false);

                            switch (fFileRef.MultimediaFormat) {
                                case GEDCOMMultimediaFormat.mfTXT:
                                    {
                                        TextArea txtBox = new TextArea();
                                        txtBox.ReadOnly = true;

                                        try {
                                            // FIXME: fix encoding! and test other!!!
                                            using (StreamReader strd = new StreamReader(fs, Encoding.GetEncoding(1251))) {
                                                txtBox.Text = strd.ReadToEnd();
                                            }
                                        } catch (Exception ex) {
                                            Logger.LogWrite("MediaViewerWin.SetFileRef.1(): " + ex.Message);
                                        }

                                        ctl = txtBox;
                                        SetViewControl(ctl);
                                    }
                                    break;

                                case GEDCOMMultimediaFormat.mfRTF:
                                    {
                                        RichTextArea rtfBox = new RichTextArea();
                                        rtfBox.ReadOnly = true;

                                        try {
                                            using (StreamReader strd = new StreamReader(fs)) {
                                                rtfBox.Text = strd.ReadToEnd();
                                            }
                                        } catch (Exception ex) {
                                            Logger.LogWrite("MediaViewerWin.SetFileRef.2(): " + ex.Message);
                                        }

                                        ctl = rtfBox;
                                        SetViewControl(ctl);
                                    }
                                    break;

                                case GEDCOMMultimediaFormat.mfHTM:
                                    {
                                        var browser = new WebView();

                                        try {
                                            browser.LoadHtml(fs);
                                            /*using (StreamReader strd = new StreamReader(fs)) {
                                                browser.DocumentText = strd.ReadToEnd();
                                                // didn't work, because didn't defines codepage from page's header (?!)
                                            }*/
                                        } catch (Exception ex) {
                                            Logger.LogWrite("MediaViewerWin.SetFileRef.3(): " + ex.Message);
                                        }

                                        ctl = browser;
                                        SetViewControl(ctl);
                                    }
                                    break;
                            }
                            if (fs != null && !(ctl is WebView))
                                fs.Dispose();
                            
                            break;
                        }
                }
            } catch (Exception ex) {
                if (ctl != null)
                    ctl.Dispose();

                Logger.LogWrite("MediaViewerWin.SetFileRef(): " + ex.Message);
            }
        }

        public void SetViewMedia(string mediaFile)
        {
            var mediaPlayer = new GKUI.Components.MediaPlayer();
            mediaPlayer.MediaFile = mediaFile;

            SetViewControl(mediaPlayer);
        }

        public void SetViewImage(Image img, GEDCOMFileReferenceWithTitle fileRef)
        {
            var imageCtl = new GKUI.Components.ImageView();
            imageCtl.OpenImage(img);
            imageCtl.btnSizeToFit.ToolTip = LangMan.LS(LSID.LSID_SizeToFit);
            imageCtl.btnZoomIn.ToolTip = LangMan.LS(LSID.LSID_ZoomIn);
            imageCtl.btnZoomOut.ToolTip = LangMan.LS(LSID.LSID_ZoomOut);

            ProcessPortraits(imageCtl, fileRef);

            fTimer = AppHost.Instance.CreateTimer(100.0f, InitViewer_Tick);
            fTimer.Start();

            SetViewControl(imageCtl);
        }

        private void ProcessPortraits(GKUI.Components.ImageView imageCtl, GEDCOMFileReferenceWithTitle fileRef)
        {
            var mmRec = fileRef.Parent as GEDCOMMultimediaRecord;

            var linksList = new List<GEDCOMObject>();
            GKUtils.SearchRecordLinks(linksList, mmRec.Owner, mmRec);

            foreach (var link in linksList) {
                var mmLink = link as GEDCOMMultimediaLink;
                if (mmLink != null && mmLink.IsPrimary) {
                    var indiRec = mmLink.Parent as GEDCOMIndividualRecord;
                    string indiName = GKUtils.GetNameString(indiRec, true, false);
                    var region = UIHelper.Rt2Rt(mmLink.CutoutPosition.Value);

                    imageCtl.NamedRegions.Add(new NamedRegion(indiName, region));
                }
            }

            imageCtl.ShowNamedRegionTips = (imageCtl.NamedRegions.Count > 0);
        }

        private void SetViewControl(Control ctl)
        {
            if (ctl != null) {
                fViewer = ctl;
                fViewer.Size = new Size(1000, 600);

                SuspendLayout();
                Content = fViewer;
                ResumeLayout();
            }
        }

        public MediaViewerWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            fBase = baseWin;
            SetLang();
        }

        public void SetLang()
        {
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);

            if (fViewer != null) {
                fViewer.Focus();
                fViewer.Invalidate();

                /*var imageViewer = fViewer as GKUI.Components.ImageView;
                if (imageViewer != null) {
                    imageViewer.ZoomToFit();
                }*/
            }
        }

        // dirty temporary hack
        private void InitViewer_Tick(object sender, EventArgs e)
        {
            var imageViewer = fViewer as GKUI.Components.ImageView;
            if (imageViewer != null && !imageViewer.Viewport.Size.IsEmpty) {
                imageViewer.ZoomToFit();
                fTimer.Stop();
            }
        }

        private void MediaViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape)
                Close();
        }

        private void MediaViewerWin_FormClosing(object sender, CancelEventArgs e)
        {
            var mediaPlayer = fViewer as MediaPlayer;
            if (mediaPlayer != null) {
                mediaPlayer.btnStop_Click(null, null);
            }
        }
    }
}
