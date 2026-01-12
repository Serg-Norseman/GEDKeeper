/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.ComponentModel;
using System.IO;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Locales;

namespace GKUI.Forms
{
    public partial class MediaViewerWin : CommonWindow, IMediaViewerWin
    {
        private readonly MediaViewerController fController;
        private Control fViewer;

        public GDMMultimediaRecord MultimediaRecord
        {
            get { return fController.MultimediaRecord; }
            set { fController.MultimediaRecord = value; }
        }

        public GDMFileReferenceWithTitle FileReference
        {
            get { return fController.FileReference; }
            set { fController.FileReference = value; }
        }

        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }

        public object ViewControl
        {
            get {
                return fViewer;
            }
        }

        public MediaViewerWin(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            SetLocale();

            fController = new MediaViewerController(this);
            fController.Init(baseWin);
        }

        public override void SetLocale()
        {
            var localizable = fViewer as ILocalizable;
            if (localizable != null) localizable.SetLocale();
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            if (fViewer != null) fViewer.Focus();
        }

        private void MediaViewerWin_FormClosing(object sender, CancelEventArgs e)
        {
#if !DIS_VLC
            var mediaPlayer = fViewer as MediaPlayer;
            if (mediaPlayer != null) {
                mediaPlayer.btnStop_Click(null, null);
            }
#endif
        }

        private void MediaViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
        }

        public void SetViewText(string text)
        {
            try {
                TextArea txtBox = new TextArea();
                txtBox.ReadOnly = true;
                // FIXME: fix encoding! and test other!!!
                txtBox.Text = text;

                SetViewControl(txtBox);
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewText()", ex);
            }
        }

        public void SetViewRTF(string text)
        {
            try {
                RichTextArea rtfBox = new RichTextArea();
                rtfBox.ReadOnly = true;
                rtfBox.Text = text;

                SetViewControl(rtfBox);
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewRTF()", ex);
            }
        }

        public void SetViewHTML(Stream stm)
        {
            try {
                var browser = new WebView();
                browser.LoadHtml(stm);

                SetViewControl(browser);
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewHTML()", ex);
            }
        }

        public void SetViewMedia(string mediaFile)
        {
#if !DIS_VLC
            var mediaPlayer = new MediaPlayer();
            mediaPlayer.MediaFile = mediaFile;

            SetViewControl(mediaPlayer);
#endif
        }

        public void SetViewImage(IImage img)
        {
            var imageCtl = new GKUI.Components.ImageView();
            imageCtl.OpenImage(fController, img);

            fController.ProcessPortraits(imageCtl);

            SetViewControl(imageCtl);
        }

        public void DisposeViewControl()
        {
            if (fViewer != null) fViewer.Dispose();
        }

        private void SetViewControl(Control ctl)
        {
            if (ctl == null) return;
            fViewer = ctl;
            SetLocale();

            SuspendLayout();
            Content = fViewer;
            ResumeLayout();
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }
    }
}
