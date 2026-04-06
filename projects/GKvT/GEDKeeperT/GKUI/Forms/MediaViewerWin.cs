/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Locales;
using Terminal.Gui;

namespace GKUI.Forms
{
    public partial class MediaViewerWin : CommonWindow, IMediaViewerWin
    {
        private readonly MediaViewerController fController;
        private View fViewer;

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

        private void MediaViewerWin_FormClosing(object sender, ToplevelClosingEventArgs e)
        {
        }

        private void MediaViewerWin_KeyDown(object sender, KeyEventEventArgs e)
        {
            if (e.KeyEvent.Key == Key.Esc) Close();
        }

        public void SetViewText(string text)
        {
            try {
                var txtBox = new TextView();
                txtBox.Multiline = true;
                txtBox.ReadOnly = true;
                txtBox.Text = text;

                SetViewControl(txtBox);
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewText()", ex);
            }
        }

        public void SetViewRTF(string text)
        {
            try {
                /*RichTextBox rtfBox = new RichTextBox();
                rtfBox.ReadOnly = true;
                rtfBox.Text = text;

                SetViewControl(rtfBox);*/
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewRTF()", ex);
            }
        }

        public void SetViewHTML(Stream stm)
        {
            // implementation is impossible
        }

        public void SetViewMedia(string mediaFile)
        {
            // implementation is impossible
        }

        public void SetViewImage(IImage img)
        {
            var imageCtl = new GKUI.Components.ImageView();
            imageCtl.OpenImage(fController, img);

            SetViewControl(imageCtl);
        }

        public void DisposeViewControl()
        {
            if (fViewer != null) fViewer.Dispose();
        }

        private void SetViewControl(View ctl)
        {
            if (ctl == null) return;
            fViewer = ctl;
            SetLocale();

            ctl.Location = new Point(0, 0);
            ctl.Height = Dim.Fill();
            ctl.Width = Dim.Fill();
            Add(ctl);
        }
    }
}
