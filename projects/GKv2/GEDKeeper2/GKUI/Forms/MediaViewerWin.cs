/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Locales;
using GKUI.Components;

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
            InitializeComponent();

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
            if (fViewer != null) fViewer.Select();
        }

        private void MediaViewerWin_FormClosing(object sender, FormClosingEventArgs e)
        {
            var mediaPlayer = fViewer as MediaPlayer;
            if (mediaPlayer != null) {
                mediaPlayer.btnStop_Click(null, null);
            }
        }

        private void MediaViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }

        public void SetViewText(string text)
        {
            try {
                TextBox txtBox = new TextBox();
                txtBox.Multiline = true;
                txtBox.ScrollBars = ScrollBars.Both;
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
                RichTextBox rtfBox = new RichTextBox();
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
                var browser = new WebBrowser();
                browser.DocumentStream = stm;

                SetViewControl(browser);
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewHTML()", ex);
            }
        }

        public void SetViewMedia(string mediaFile)
        {
            var mediaPlayer = new MediaPlayer();
            mediaPlayer.MediaFile = mediaFile;

            SetViewControl(mediaPlayer);
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

            ctl.Dock = DockStyle.Fill;
            ctl.Location = new Point(0, 0);
            ctl.Size = new Size(100, 100);
            Controls.Add(ctl);
            Controls.SetChildIndex(ctl, 0);

            ResumeLayout(false);
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }
    }
}
