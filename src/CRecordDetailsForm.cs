/* CRecordDetailsForm.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using GEDmill.LLClasses;
using System.IO;

namespace GEDmill
{
    // A form that shows extra information about a record, including any associated picture
    public class CRecordDetailsForm : System.Windows.Forms.Form
    {
        // Windows controls
        private System.Windows.Forms.PictureBox m_picturebox;
        private System.Windows.Forms.Button m_buttonPictures;
        private System.Windows.Forms.Label m_labelNoPicture;
        private System.Windows.Forms.Button m_buttonOk;
        private System.Windows.Forms.Label m_labelDetailsText;
        private System.Windows.Forms.TextBox m_textbox;

        // The parsed GEDCOM. Required for PicturesForm
        private CGedcom m_gedcom;

        // The record in question
        private CISRecord m_record;

        // Size for the record's picture box
        private Size m_sizePictureBox;

        // Title for the picture box
        private System.Windows.Forms.Label m_labelTitle;

        // True if user is allowed to select the text on the form
        private bool m_bCanSelectText;

        // Required designer variable.
        private System.ComponentModel.Container components = null;

        // Constructor
        public CRecordDetailsForm( IWin32Window parent, CISRecord r, CGedcom gedcomParser, bool bCanEditPictures, bool bCanSelectText )
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();
            m_bCanSelectText = bCanSelectText;
            m_gedcom = gedcomParser;
            m_record = r;

            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();
            m_buttonPictures.Visible = bCanEditPictures;

            m_sizePictureBox = m_picturebox.Size;
        
            if( r == null )
            {
                return;
            }

            string sDetails = m_record.Details();

            if( m_record is CSourceRecord )
            {
                sDetails += "\r\nReferenced by: ";
                string sBackref = "";
                Hashtable htBackrefs = ((CSourceRecord)m_record).MakeBackRefList();

                IDictionaryEnumerator ide = htBackrefs.GetEnumerator();
                while( ide.MoveNext() )
                {
                    CIndividualRecord ir = (CIndividualRecord)(ide.Value);
                    if( ir != null )
                    {


                        string sFirstName = "";
                        string sSurname = "";
                        string sName = MainForm.s_config.CapitaliseName( ir.Name, ref sFirstName, ref sSurname );
                        if( sName == "" )
                        {
                            sName = MainForm.s_config.m_sUnknownName;
                        }

                        if( sName != "" )
                        {
                            if( sBackref != "" )
                            {
                                sBackref += ", ";
                            }
                            sBackref += sName;
                        }
                    }
                }
                if( sBackref == "" )
                {
                    sDetails += "no references";
                }
                else
                {
                    sDetails += sBackref;
                }
            }

            if( m_bCanSelectText )
            {
                m_textbox.Text = sDetails;
                m_textbox.SelectionLength = 0;
            }
            else
            {
                m_labelDetailsText.Text = sDetails;
            }

            // Select first picture
            SetPicture();

            m_buttonOk.Select(); // To avoid flashing cursor appearing in text box when window appears.

            Cursor.Current = Cursors.Default;
            Cursor.Show();
        }

        // Selects first picture associated with the record to display in the form
        protected void SetPicture()
        {
            CMultimediaFileReference mfrFirst = null;

            foreach( CMultimediaFileReference mfr in m_record.m_alUniqueFileRefs )
            {
                if( mfr.m_bVisible && (mfrFirst == null || mfrFirst.m_nOrderIndex > mfr.m_nOrderIndex) )
                {
                    mfrFirst = mfr;
                }
            }

            string sMmTitle = "";
            string sMmFilename = "";
            string sMmFormat = "";
            bool bIsNonPic = true;
            Rectangle rectArea = new Rectangle( 0,0,0,0 );
            if( mfrFirst != null )
            {
                sMmTitle = mfrFirst.m_sDescriptiveTitle;
                sMmFilename = mfrFirst.m_sMultimediaFileReference;
                sMmFormat = mfrFirst.m_sMultimediaFormat;
                bIsNonPic = !mfrFirst.IsPictureFormat();
                if( mfrFirst.m_asidPair != null )
                {
                    rectArea = mfrFirst.m_asidPair.m_rectArea;
                }
            }
            
            ShowPicture( sMmFilename, sMmTitle, sMmFormat, bIsNonPic, m_labelNoPicture, m_labelTitle, m_picturebox, m_sizePictureBox, rectArea );

        }

        // Clean up any resources being used.
        protected override void Dispose( bool bDisposing )
        {
            if( bDisposing )
            {
                if(components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose( bDisposing );
        }

        // Required method for Designer support
        private void InitializeComponent()
        {
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(CRecordDetailsForm));
            this.m_picturebox = new System.Windows.Forms.PictureBox();
            this.m_buttonPictures = new System.Windows.Forms.Button();
            this.m_buttonOk = new System.Windows.Forms.Button();
            this.m_labelNoPicture = new System.Windows.Forms.Label();
            this.m_labelTitle = new System.Windows.Forms.Label();
            this.m_textbox = new System.Windows.Forms.TextBox();
            this.m_labelDetailsText = new System.Windows.Forms.Label();
            this.SuspendLayout();

            // 
            // pictureBox1
            // 
            this.m_picturebox.Location = new System.Drawing.Point(24, 16);
            this.m_picturebox.Name = "pictureBox1";
            this.m_picturebox.Size = new System.Drawing.Size(96, 104);
            this.m_picturebox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.m_picturebox.TabIndex = 0;
            this.m_picturebox.TabStop = false;

            // 
            // picturesButton
            // 
            this.m_buttonPictures.Location = new System.Drawing.Point(32, 168);
            this.m_buttonPictures.Name = "picturesButton";
            this.m_buttonPictures.TabIndex = 2;
            this.m_buttonPictures.Text = "&Pictures...";
            this.m_buttonPictures.Click += new System.EventHandler(this.picturesButton_Click);

            // 
            // okbutton
            // 
            this.m_buttonOk.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.m_buttonOk.Location = new System.Drawing.Point(353, 168);
            this.m_buttonOk.Name = "okbutton";
            this.m_buttonOk.Size = new System.Drawing.Size(74, 23);
            this.m_buttonOk.TabIndex = 4;
            this.m_buttonOk.Text = "OK";

            // 
            // noPictureLabel
            // 
            this.m_labelNoPicture.Location = new System.Drawing.Point(8, 16);
            this.m_labelNoPicture.Name = "noPictureLabel";
            this.m_labelNoPicture.Size = new System.Drawing.Size(120, 100);
            this.m_labelNoPicture.TabIndex = 5;
            this.m_labelNoPicture.Text = "no picture";
            this.m_labelNoPicture.TextAlign = System.Drawing.ContentAlignment.TopCenter;

            // 
            // titleLabel
            // 
            this.m_labelTitle.Font = new System.Drawing.Font("Times New Roman", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            this.m_labelTitle.Location = new System.Drawing.Point(8, 136);
            this.m_labelTitle.Name = "titleLabel";
            this.m_labelTitle.Size = new System.Drawing.Size(120, 32);
            this.m_labelTitle.TabIndex = 6;
            this.m_labelTitle.TextAlign = System.Drawing.ContentAlignment.TopCenter;

            // 
            // textText
            // 
            this.m_textbox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.m_textbox.Location = new System.Drawing.Point(136, 16);
            this.m_textbox.Multiline = true;
            this.m_textbox.Name = "textText";
            this.m_textbox.ReadOnly = true;
            this.m_textbox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.m_textbox.Size = new System.Drawing.Size(288, 144);
            this.m_textbox.TabIndex = 1;
            this.m_textbox.Text = "";

            // 
            // detailsText
            // 
            this.m_labelDetailsText.Location = new System.Drawing.Point(136, 16);
            this.m_labelDetailsText.Name = "detailsText";
            this.m_labelDetailsText.Size = new System.Drawing.Size(288, 120);
            this.m_labelDetailsText.TabIndex = 1;

            // 
            // CRecordDetailsForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(439, 200);
            this.Controls.Add(this.m_labelTitle);
            this.Controls.Add(this.m_labelNoPicture);
            this.Controls.Add(this.m_buttonOk);
            this.Controls.Add(this.m_buttonPictures);
            this.Controls.Add(this.m_picturebox);
            if( m_bCanSelectText )
            {
                this.Controls.Add(this.m_textbox);

            }
            else
            {
                this.Controls.Add(this.m_labelDetailsText);
            }
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "CRecordDetailsForm";
            this.ShowInTaskbar = false;
            this.Text = "Record Details";
            this.ResumeLayout(false);

        }

        // Bring up the form that lets user add/remove/order pictures for this record
        private void picturesButton_Click(object sender, System.EventArgs e)
        {
            if( m_record != null )
            {
                PicturesForm picForm = new PicturesForm( m_record, m_gedcom );
                picForm.ShowDialog( this );
                SetPicture(); // User may have put another picture first.
            }
        }
        
        // Load and display the selected picture for this record
        public static void ShowPicture( string sMmFilename, string sMmTitle, string sMmFormat, bool bNonPic, System.Windows.Forms.Label labelNoPicture, System.Windows.Forms.Label labelTitle, System.Windows.Forms.PictureBox pictureBox, Size sizePictureBox, Rectangle rectArea )
        {
            if( labelTitle != null )
            {
                labelTitle.Text = sMmTitle;
            }

            if( sMmFilename != null && sMmFilename.Length == 0 )
            {
                labelNoPicture.Text = "no picture";
            }
            else
            {
                labelNoPicture.Text = "can't load picture "+sMmFilename;
            }
            labelTitle.Top = labelNoPicture.Bottom + 4;

            bool bNoPictureLabelVisible = true;
            bool bPictureBoxVisible = false;

            if( File.Exists( sMmFilename ) )
            {
                if( bNonPic )
                {
                    sMmFilename = MainForm.s_config.m_sApplicationPath + "\\" + MainForm.NonPicFilename( sMmFormat, true, false );
                }
                try
                {
                    Image pic = Image.FromFile( sMmFilename );
                    SizeF size = pic.PhysicalDimension;
                    //always for now, until can crop image to sAsid sArea : if( sArea.Width == 0 && sArea.Height == 0 )
                    {
                        rectArea = new Rectangle( 0,0, (int)size.Width, (int)size.Height );
                    }
                    MainForm.ScaleAreaToFit( ref rectArea, (uint)sizePictureBox.Width, (uint)sizePictureBox.Height );
                    pictureBox.Width = rectArea.Width;
                    pictureBox.Height = rectArea.Height;
                    pictureBox.Image = pic;
                    labelTitle.Top = pictureBox.Bottom + 4;

                    // Loaded successfully
                    bNoPictureLabelVisible = false;
                    bPictureBoxVisible = true;

                }
                catch( Exception )
                {
                }
            }

            pictureBox.Visible = bPictureBoxVisible;
            labelNoPicture.Visible = bNoPictureLabelVisible;

        }

    }
}
