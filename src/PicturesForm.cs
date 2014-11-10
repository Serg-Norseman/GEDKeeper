/* PicturesForm.cs
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
    // Form that lets user add/remove/re-order pictures for a record.
    public class PicturesForm : System.Windows.Forms.Form
    {
        // Windows controls
        private System.Windows.Forms.ListView m_listviewPictures;
        private System.Windows.Forms.Button m_buttonUp;
        private System.Windows.Forms.Button m_buttonDown;
        private System.Windows.Forms.Button m_buttonRemove;
        private System.Windows.Forms.Button m_buttonAdd;
        private System.Windows.Forms.PictureBox m_picturebox;
        private System.Windows.Forms.Label m_labelPicture;
        private System.Windows.Forms.Button m_buttonOk;
        private System.Windows.Forms.Button m_buttonCancel;
        private System.Windows.Forms.Label m_labelNoPictures;
        private System.Windows.Forms.Label m_label1;

        // Required designer variable.
        private System.ComponentModel.Container components = null;

        // List of all the pictures
        private ArrayList m_alMultimediaFileRefs;

        // Size of the picture box
        private Size m_sizePictureBox;

        // The parsed GEDCOM. Required because we create a new CMultimediaFileReference which is a Gedcom parser object and needs a parent.
        private CGedcom m_gedcom;

        // The record that the picture belongs to
        private CISRecord m_record;

        // Indicates user has made changes to data from GEDCOM file. Local copy sets MainForm copy on dialog ok.
        private bool m_bExtraUserInfoAdded;

        // As system builds list it generates check events. We don't want these to signify user-initiated changes.
        private bool m_bDisablePictureListCheckEvent; 


        // Constructor
        public PicturesForm( CISRecord r, CGedcom gedcom )
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_bExtraUserInfoAdded = false;
            m_bDisablePictureListCheckEvent = true;
            m_record = r;
            // Add pictures
            // Copy MFRs to a new list
            m_alMultimediaFileRefs = new ArrayList();
            foreach( CMultimediaFileReference mfr in m_record.m_alUniqueFileRefs )
            {
                CAsidPair mmasidPair = mfr.m_asidPair;
                CMultimediaFileReference new_mfr = new CMultimediaFileReference( mfr, true ); // Make copy
                m_alMultimediaFileRefs.Add( new_mfr );
            }

            m_gedcom = gedcom;

            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            m_listviewPictures.View = View.Details;
            m_listviewPictures.Columns.Add("Title", (m_listviewPictures.Width-6) * 40 / 100, HorizontalAlignment.Left);
            m_listviewPictures.Columns.Add("Filename", (m_listviewPictures.Width-6) * 60 / 100, HorizontalAlignment.Left);

            m_sizePictureBox = m_picturebox.Size;
        
            // Add pictures
            FillPicturesList( m_listviewPictures, m_alMultimediaFileRefs );
            EnableButtons();

            // Select first checked picture
            int index = 0;
            foreach( CListableMFR lmfr in m_listviewPictures.Items )
            {
                if( lmfr.Checked )
                {
                    index = lmfr.Index;
                    break;
                }
            }
            SetPicture(index);

            Cursor.Current = Cursors.Default;
            Cursor.Show();
        }

        // Fill the list control with a list of all multimedia files associated with the record
        private static void FillPicturesList( ListView listView, ArrayList alMultimediaFileRefs )
        {
            listView.SuspendLayout();

            LogFile.TheLogFile.WriteLine( LogFile.DT_APP, LogFile.EDebugLevel.Note, "FillPicturesList() : " + alMultimediaFileRefs.Count.ToString() );

            CMultimediaFileReference mfrSelected = null;
            CListableMFR lmfr = GetSelectedItem( listView );
            if( lmfr != null )
            {
                mfrSelected = lmfr.MultimediaFileReference;
            }

            alMultimediaFileRefs.Sort( new CMultimediaFileReference.OrderComparer() );

            string sMmFilename;
        
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            listView.Items.Clear();

            ListViewItem[] temporaryItemsList = new ListViewItem[ alMultimediaFileRefs.Count ];

            int nItem = 0;
            foreach( CMultimediaFileReference mfr in alMultimediaFileRefs )
            {               
                if( mfr.m_bEmbedded )
                {
                    sMmFilename = "embedded media";
                }
                else
                {
                    sMmFilename = mfr.m_sMultimediaFileReference;
                }

                CListableMFR item = new CListableMFR( mfr );
                if( mfrSelected == mfr )
                {
                    item.Selected = true;
                }

                if( mfr.m_bVisible )
                {
                    item.Checked = true;
                }
                else
                {
                    item.Checked = false;
                }

                if( sMmFilename != null && sMmFilename.Length > 0)
                {
                    item.SubItems.Add( sMmFilename );
                }

                temporaryItemsList[ nItem++ ] = item;
            }

            listView.Items.AddRange( temporaryItemsList );

            listView.ResumeLayout();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();
        }

        // Clean up any resources being used.
        protected override void Dispose( bool disposing )
        {
            if( disposing )
            {
                if(components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose( disposing );
        }

        // Required method for Designer support 
        private void InitializeComponent()
        {
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(PicturesForm));
            this.m_listviewPictures = new System.Windows.Forms.ListView();
            this.m_buttonUp = new System.Windows.Forms.Button();
            this.m_buttonDown = new System.Windows.Forms.Button();
            this.m_buttonRemove = new System.Windows.Forms.Button();
            this.m_buttonAdd = new System.Windows.Forms.Button();
            this.m_picturebox = new System.Windows.Forms.PictureBox();
            this.m_labelPicture = new System.Windows.Forms.Label();
            this.m_buttonOk = new System.Windows.Forms.Button();
            this.m_buttonCancel = new System.Windows.Forms.Button();
            this.m_labelNoPictures = new System.Windows.Forms.Label();
            this.m_label1 = new System.Windows.Forms.Label();
            this.SuspendLayout();

            // 
            // pictureList
            // 
            this.m_listviewPictures.AllowColumnReorder = true;
            this.m_listviewPictures.AutoArrange = false;
            this.m_listviewPictures.CheckBoxes = true;
            this.m_listviewPictures.FullRowSelect = true;
            this.m_listviewPictures.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
            this.m_listviewPictures.HideSelection = false;
            this.m_listviewPictures.LabelEdit = true;
            this.m_listviewPictures.Location = new System.Drawing.Point(16, 187);
            this.m_listviewPictures.MultiSelect = false;
            this.m_listviewPictures.Name = "pictureList";
            this.m_listviewPictures.Size = new System.Drawing.Size(352, 104);
            this.m_listviewPictures.TabIndex = 2;
            this.m_listviewPictures.View = System.Windows.Forms.View.Details;
            this.m_listviewPictures.AfterLabelEdit += new System.Windows.Forms.LabelEditEventHandler(this.pictureList_AfterLabelEdit);
            this.m_listviewPictures.SelectedIndexChanged += new System.EventHandler(this.pictureList_SelectedIndexChanged);
            this.m_listviewPictures.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(this.pictureList_ItemCheck);

            // 
            // upButton
            // 
            this.m_buttonUp.Location = new System.Drawing.Point(384, 243);
            this.m_buttonUp.Name = "upButton";
            this.m_buttonUp.Size = new System.Drawing.Size(76, 20);
            this.m_buttonUp.TabIndex = 5;
            this.m_buttonUp.Text = "Move &up";
            this.m_buttonUp.Click += new System.EventHandler(this.upButton_Click);

            // 
            // downButton
            // 
            this.m_buttonDown.Location = new System.Drawing.Point(384, 270);
            this.m_buttonDown.Name = "downButton";
            this.m_buttonDown.Size = new System.Drawing.Size(76, 20);
            this.m_buttonDown.TabIndex = 6;
            this.m_buttonDown.Text = "Move &down";
            this.m_buttonDown.Click += new System.EventHandler(this.downButton_Click);

            // 
            // removeButton
            // 
            this.m_buttonRemove.Location = new System.Drawing.Point(384, 215);
            this.m_buttonRemove.Name = "removeButton";
            this.m_buttonRemove.Size = new System.Drawing.Size(76, 20);
            this.m_buttonRemove.TabIndex = 4;
            this.m_buttonRemove.Text = "&Remove";
            this.m_buttonRemove.Click += new System.EventHandler(this.removeButton_Click);

            // 
            // addButton
            // 
            this.m_buttonAdd.Location = new System.Drawing.Point(384, 187);
            this.m_buttonAdd.Name = "addButton";
            this.m_buttonAdd.Size = new System.Drawing.Size(76, 20);
            this.m_buttonAdd.TabIndex = 3;
            this.m_buttonAdd.Text = "&Add...";
            this.m_buttonAdd.Click += new System.EventHandler(this.addButton_Click);

            // 
            // pictureBox
            // 
            this.m_picturebox.Location = new System.Drawing.Point(16, 16);
            this.m_picturebox.Name = "pictureBox";
            this.m_picturebox.Size = new System.Drawing.Size(440, 136);
            this.m_picturebox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.m_picturebox.TabIndex = 0;
            this.m_picturebox.TabStop = false;

            // 
            // pictureLabel
            // 
            this.m_labelPicture.Font = new System.Drawing.Font("Times New Roman", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            this.m_labelPicture.Location = new System.Drawing.Point(16, 160);
            this.m_labelPicture.Name = "pictureLabel";
            this.m_labelPicture.Size = new System.Drawing.Size(440, 23);
            this.m_labelPicture.TabIndex = 1;
            this.m_labelPicture.Text = "label1";
            this.m_labelPicture.TextAlign = System.Drawing.ContentAlignment.TopCenter;

            // 
            // okbutton
            // 
            this.m_buttonOk.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.m_buttonOk.Location = new System.Drawing.Point(397, 305);
            this.m_buttonOk.Name = "okbutton";
            this.m_buttonOk.Size = new System.Drawing.Size(63, 20);
            this.m_buttonOk.TabIndex = 8;
            this.m_buttonOk.Text = "OK";
            this.m_buttonOk.Click += new System.EventHandler(this.okbutton_Click);

            // 
            // cancelbutton
            // 
            this.m_buttonCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.m_buttonCancel.Location = new System.Drawing.Point(327, 305);
            this.m_buttonCancel.Name = "cancelbutton";
            this.m_buttonCancel.Size = new System.Drawing.Size(62, 20);
            this.m_buttonCancel.TabIndex = 7;
            this.m_buttonCancel.Text = "Cancel";

            // 
            // noPictureLabel
            // 
            this.m_labelNoPictures.Location = new System.Drawing.Point(16, 69);
            this.m_labelNoPictures.Name = "noPictureLabel";
            this.m_labelNoPictures.Size = new System.Drawing.Size(448, 70);
            this.m_labelNoPictures.TabIndex = 9;
            this.m_labelNoPictures.Text = "no picture";
            this.m_labelNoPictures.TextAlign = System.Drawing.ContentAlignment.TopCenter;

            // 
            // label1
            // 
            this.m_label1.Location = new System.Drawing.Point(20, 291);
            this.m_label1.Name = "label1";
            this.m_label1.Size = new System.Drawing.Size(300, 35);
            this.m_label1.TabIndex = 10;
            this.m_label1.Text = "Tick the box next to the pictures you want to include in the website. Click the title to edit it.";

            // 
            // PicturesForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(471, 334);
            this.Controls.Add(this.m_label1);
            this.Controls.Add(this.m_labelNoPictures);
            this.Controls.Add(this.m_buttonCancel);
            this.Controls.Add(this.m_buttonOk);
            this.Controls.Add(this.m_labelPicture);
            this.Controls.Add(this.m_picturebox);
            this.Controls.Add(this.m_buttonAdd);
            this.Controls.Add(this.m_buttonRemove);
            this.Controls.Add(this.m_buttonDown);
            this.Controls.Add(this.m_buttonUp);
            this.Controls.Add(this.m_listviewPictures);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "PicturesForm";
            this.ShowInTaskbar = false;
            this.Text = "Select Pictures";
            this.Activated += new System.EventHandler(this.PicturesForm_Activated);
            this.ResumeLayout(false);

        }

        // Display the given picture from the list
        protected void SetPicture( int nPic )
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            string sMmTitle = "";
            string sMmFilename = "";
            string sMmFormat = "";
            bool bNonPic = true;
            Rectangle rectArea = new Rectangle( 0,0,0,0 );
            if( m_alMultimediaFileRefs.Count > nPic )
            {
                CListableMFR lmfr = (CListableMFR)m_listviewPictures.Items[ nPic ];
                CMultimediaFileReference mfr = lmfr.MultimediaFileReference;
                sMmTitle = mfr.m_sDescriptiveTitle;
                sMmFilename = mfr.m_sMultimediaFileReference;
                sMmFormat = mfr.m_sMultimediaFormat;
                bNonPic = !mfr.IsPictureFormat();
                if( mfr.m_asidPair != null )
                {
                    rectArea = mfr.m_asidPair.m_rectArea;
                }
            }
            
            CRecordDetailsForm.ShowPicture( sMmFilename, sMmTitle, sMmFormat, bNonPic, m_labelNoPictures, m_labelPicture, m_picturebox, m_sizePictureBox, rectArea );

            // Centre picture box
            m_picturebox.Left = (this.Width - m_picturebox.Width) / 2;

            Cursor.Current = Cursors.Default;
            Cursor.Hide();
        }

        // Event handler for the add pictures button
        private void addButton_Click(object sender, System.EventArgs e)
        {
            m_bExtraUserInfoAdded = true;
            string sFileName = "";
            string sFileDir = MainForm.s_config.m_sLastPictureAddedDir;
            if( sFileDir.Length == 0 || Directory.Exists( sFileDir ) == false )
            {
                // Use path from input GEDCOM file
                sFileDir = Path.GetDirectoryName( MainForm.m_mainForm.InputFile ); 
            }
            ArrayList alFilterExtensions = new ArrayList();
            alFilterExtensions.Add( ".jpg" );
            alFilterExtensions.Add( ".jpeg" );
            alFilterExtensions.Add( ".gif" );
            alFilterExtensions.Add( ".bmp" );
            alFilterExtensions.Add( ".png" );
            if( MainForm.SelectFile( ref sFileDir, ref sFileName, "Select picture", "", true, "Picture files", alFilterExtensions ) )
            {
                MainForm.s_config.m_sLastPictureAddedDir = sFileDir;

                CMultimediaFileReference mfr = new CMultimediaFileReference( m_gedcom );
                mfr.m_sDescriptiveTitle = "";

                mfr.m_sMultimediaFileReference = sFileDir + "\\" + sFileName;
                mfr.m_bFromGEDCOM = false;
                mfr.m_bVisible = true;
                mfr.m_nOrderIndex = m_listviewPictures.Items.Count;
                m_alMultimediaFileRefs.Add( mfr );

                FillPicturesList( m_listviewPictures, m_alMultimediaFileRefs );
                EnableButtons();
            }
        }

        // Event handler for the remove picture button
        private void removeButton_Click(object sender, System.EventArgs e)
        {
            m_bExtraUserInfoAdded = true;
            ListView.SelectedListViewItemCollection selected_items = m_listviewPictures.SelectedItems;
            if( selected_items.Count > 0 )
            {
                foreach( CListableMFR lmfr in selected_items )
                {
                    // Delete selected mfrs from m_multimediaFileRefs
                    CMultimediaFileReference mfr = lmfr.MultimediaFileReference;
                    m_alMultimediaFileRefs.Remove( mfr );
                }
                FillPicturesList( m_listviewPictures, m_alMultimediaFileRefs );
                int nSelected = EnableButtons();
                SetPicture( nSelected>=0?nSelected:0 );
            }
        }

        // Event handler for the move picture order up button
        private void upButton_Click(object sender, System.EventArgs e)
        {
            m_bExtraUserInfoAdded = true;
            m_listviewPictures.SuspendLayout();

            CListableMFR lmfr = GetSelectedItem( m_listviewPictures );
            if( lmfr != null )
            {
                int nSelIndex = lmfr.Index;
                if( nSelIndex > 0 )
                {
                    CListableMFR lmfrOther = (CListableMFR)m_listviewPictures.Items[ nSelIndex-1 ];
                    if( lmfrOther != null )
                    {
                        // Swap the two order values
                        int temp = lmfrOther.MultimediaFileReference.m_nOrderIndex;
                        lmfrOther.MultimediaFileReference.m_nOrderIndex = lmfr.MultimediaFileReference.m_nOrderIndex;
                        lmfr.MultimediaFileReference.m_nOrderIndex = temp;

                        // Rebuild the list
                        ListViewItem temp_item = m_listviewPictures.Items[nSelIndex];
                        m_listviewPictures.Items.RemoveAt(nSelIndex);
                        temp_item.Selected = true;
                        m_listviewPictures.Items.Insert( nSelIndex-1, temp_item );
                        temp_item.EnsureVisible();
                        m_listviewPictures.Focus();
                    }
                }
            }

            m_listviewPictures.ResumeLayout();
        }

        // Event handler for the move picture order down button
        private void downButton_Click(object sender, System.EventArgs e)
        {
            m_bExtraUserInfoAdded = true;
            m_listviewPictures.SuspendLayout();

            CListableMFR lmfr = GetSelectedItem( m_listviewPictures );
            if( lmfr != null )
            {
                int nSelectedIndex = lmfr.Index;
                if( nSelectedIndex < m_listviewPictures.Items.Count - 1 )
                {
                    CListableMFR lmfrOther = (CListableMFR)m_listviewPictures.Items[ nSelectedIndex+1 ];
                    if( lmfrOther != null )
                    {
                        // Swap the two order values
                        int nTemp = lmfrOther.MultimediaFileReference.m_nOrderIndex;
                        lmfrOther.MultimediaFileReference.m_nOrderIndex = lmfr.MultimediaFileReference.m_nOrderIndex;
                        lmfr.MultimediaFileReference.m_nOrderIndex = nTemp;

                        ListViewItem temp_item = m_listviewPictures.Items[nSelectedIndex];
                        m_listviewPictures.Items.RemoveAt(nSelectedIndex);
                        m_listviewPictures.Items.Insert( nSelectedIndex+1, temp_item );
                        temp_item.Selected = true;
                        temp_item.EnsureVisible();
                        m_listviewPictures.Focus();
                    }
                }
            }

            m_listviewPictures.ResumeLayout();
        }

        // Event handler for when a different picture is selected from the list
        private void pictureList_SelectedIndexChanged(object sender, System.EventArgs e)
        {
            int nSelected = EnableButtons();
            SetPicture( nSelected>=0?nSelected:0 );
        }

        // Enables buttons. As a useful side effect returns index of selected item.
        private int EnableButtons()
        {
            int nSelected = -1;
            CMultimediaFileReference mfr = null;
            CListableMFR lmfr = GetSelectedItem( m_listviewPictures );
            if( lmfr != null )
            {
                nSelected = lmfr.Index;
                mfr = lmfr.MultimediaFileReference;
            }
            
            if( mfr != null )
            {
                m_buttonRemove.Enabled = !mfr.m_bFromGEDCOM;
            }
            else
            {
                m_buttonRemove.Enabled = false;
            }
            m_buttonUp.Enabled = nSelected > 0;
            m_buttonDown.Enabled = nSelected >= 0 && nSelected < (m_listviewPictures.Items.Count)-1;

            return nSelected;
        }

        // Returns the MFR for the selected item in the list
        private static CListableMFR GetSelectedItem( ListView listView )
        {
            CListableMFR lmfr = null;
            ListView.SelectedListViewItemCollection icSelected = listView.SelectedItems;
            if( icSelected.Count > 0 )
            {
                lmfr = (CListableMFR)icSelected[0];
            }
            return lmfr;
        }

        // Event handler for when a picture item in the list is ticked
        private void pictureList_ItemCheck(object sender, System.Windows.Forms.ItemCheckEventArgs e)
        {
            if( !m_bDisablePictureListCheckEvent )
            {
                m_bExtraUserInfoAdded = true;
            }
            CListableMFR lmfr = (CListableMFR)m_listviewPictures.Items[ e.Index ];
            if( lmfr != null )
            {
                lmfr.MultimediaFileReference.m_bVisible = (e.NewValue == CheckState.Checked);
            }
        }

        // Event handler for the OK button.
        private void okbutton_Click(object sender, System.EventArgs e)
        {
            m_record.m_alUniqueFileRefs.Clear();

            // Copy our temporary MFRs back to their frParents
            foreach( CMultimediaFileReference mfr in m_alMultimediaFileRefs )
            {
                if( mfr.m_mfrOriginal != null )
                {
                    mfr.m_mfrOriginal.CopyFrom( mfr );
                    m_record.m_alUniqueFileRefs.Add( mfr.m_mfrOriginal );
                }
                else if( mfr.m_bFromGEDCOM == false )
                {
                    // Object is a new one added by user
                    m_record.m_alUniqueFileRefs.Add( mfr );
                }
            }
            
            if( m_bExtraUserInfoAdded )
            {
                MainForm.m_mainForm.m_bPrunepanelDataChanged = true;
            }
        }

        // Event handler for the text in the picture list item being changed
        private void pictureList_AfterLabelEdit(object sender, System.Windows.Forms.LabelEditEventArgs e)
        {
            m_bExtraUserInfoAdded = true;
            CListableMFR lmfr = (CListableMFR)m_listviewPictures.Items[ e.Item ];
            if( lmfr != null )
            {
                lmfr.MultimediaFileReference.m_sDescriptiveTitle = e.Label;
            }
            m_labelPicture.Text = e.Label;
        }

        // Event handler for the form being activated
        private void PicturesForm_Activated(object sender, System.EventArgs e)
        {
            m_bDisablePictureListCheckEvent = false;        
        }

    }
}

