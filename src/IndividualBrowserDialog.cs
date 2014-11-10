/* IndividualBrowserDialog.cs
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

namespace GEDmill
{
    // Form that lists all individuals and lets the user select individuals.
    public class IndividualBrowserDialog : System.Windows.Forms.Form
    {
        // The list of indidivuals
        public SortableListView m_sortableListView;

        // The OK button
        private System.Windows.Forms.Button m_buttonOk;

        // The Quit button
        private System.Windows.Forms.Button m_buttonQuit;

        // Label describing the form
        private System.Windows.Forms.Label m_label1;

        // Context menu for a highlighted individual
        private ContextMenu m_contextmenuChooseIndi;

        // Menu item that lets user see details of the highlighted individual
        private MenuItem m_menuitemChooseIndiMenuDetails;

        // The actual form
        private MainForm m_mainForm;

        // Required designer variable.
        private System.ComponentModel.Container components = null;

        // Constructor
        public IndividualBrowserDialog( MainForm mainForm, bool bMultiSelect )
        {
            m_mainForm = mainForm;

            //
            // Required for Windows Form Designer support
            //
            InitializeComponent( bMultiSelect );

            this.m_contextmenuChooseIndi = new ContextMenu();

            //
            // Choose individual context menu
            //
            m_menuitemChooseIndiMenuDetails = new MenuItem( "&Details...", new System.EventHandler(this.chooseIndiContextMenuDetails_Click ) );
            this.m_contextmenuChooseIndi.MenuItems.Add( m_menuitemChooseIndiMenuDetails );
            this.m_contextmenuChooseIndi.Popup += new EventHandler(this.chooseIndiContextMenu_popup);

            // 
            // listView1
            // 
            this.m_sortableListView = new SortableListView();
            this.m_sortableListView.Location = new System.Drawing.Point(16, 32);
            this.m_sortableListView.Name = "listView1";
            this.m_sortableListView.Size = new System.Drawing.Size(this.Width-32, 192);
            this.m_sortableListView.TabIndex = 2;
            this.m_sortableListView.View = System.Windows.Forms.View.Details;
            this.m_sortableListView.ColumnClick += new System.Windows.Forms.ColumnClickEventHandler(this.m_sortableListView.ColumnClickHandler);
            this.m_sortableListView.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right | System.Windows.Forms.AnchorStyles.Left )));
            this.m_sortableListView.MultiSelect = bMultiSelect;
            this.m_sortableListView.SelectedIndexChanged += new System.EventHandler(this.listView1_selectionChanged);
            this.m_sortableListView.AllowColumnReorder = true;
            this.m_sortableListView.GridLines = true;               
            this.m_sortableListView.ContextMenu = m_contextmenuChooseIndi;
            this.Controls.Add(this.m_sortableListView);

            m_buttonOk.Enabled = false;

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

        // Build the form
        private void InitializeComponent( bool bMultiSelect )
        {
            this.m_buttonOk = new System.Windows.Forms.Button();
            this.m_buttonQuit = new System.Windows.Forms.Button();
            this.m_label1 = new System.Windows.Forms.Label();
            this.SuspendLayout();

            // 
            // OKButton
            // 
            this.m_buttonOk.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.m_buttonOk.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.m_buttonOk.Location = new System.Drawing.Point(276, 268);
            this.m_buttonOk.Name = "OKButton";
            this.m_buttonOk.Size = new System.Drawing.Size(90, 26);
            this.m_buttonOk.TabIndex = 4;
            this.m_buttonOk.Text = "OK";

            // 
            // QuitButton
            // 
            this.m_buttonQuit.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.m_buttonQuit.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.m_buttonQuit.Location = new System.Drawing.Point(180, 268);
            this.m_buttonQuit.Name = "QuitButton";
            this.m_buttonQuit.Size = new System.Drawing.Size(86, 26);
            this.m_buttonQuit.TabIndex = 3;
            this.m_buttonQuit.Text = "Cancel";

            // 
            // label1
            // 
            this.m_label1.Location = new System.Drawing.Point(19, 9);
            this.m_label1.Name = "label1";
            this.m_label1.Size = new System.Drawing.Size(307, 19);
            this.m_label1.TabIndex = 1;
            if( bMultiSelect )
            {
                this.m_label1.Text = "&Select one or more individuals from the list:";
            }
            else
            {
                this.m_label1.Text = "&Select an individual from the list:";
            }

            // 
            // IndividualBrowserDialog
            // 
            this.AcceptButton = this.m_buttonOk;
            this.AutoScaleMode = AutoScaleMode.None;
            this.AutoScaleBaseSize = new System.Drawing.Size(6, 15);
            this.CancelButton = this.m_buttonQuit;
            this.ClientSize = new System.Drawing.Size(379, 336);
            this.ControlBox = false;
            this.Controls.Add(this.m_label1);
            this.Controls.Add(this.m_buttonQuit);
            this.Controls.Add(this.m_buttonOk);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.MinimumSize = new System.Drawing.Size(300, 300);
            this.Name = "IndividualBrowserDialog";
            this.ShowInTaskbar = false;
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Show;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Select Individual";
            this.ResumeLayout(false);
        }

        // Returns the ISRecord for the first individual selected in the list
        public LLClasses.CIndividualRecord FirstSelectedIndividual
        {
            get
            {
                if( m_sortableListView.SelectedItems.Count >= 1 )
                {
                    ListViewItem li = m_sortableListView.SelectedItems[0];
                    if( li is CListableBool )
                    {
                        return (LLClasses.CIndividualRecord)((CListableBool)((ListViewItem)li)).ISRecord;
                    }
                }
                return null;
            }
        }

        // Selection change event handler
        private void listView1_selectionChanged(object sender, System.EventArgs e)
        {
            int nSelected = m_sortableListView.SelectedItems.Count;
            m_buttonOk.Enabled = (nSelected!=0);
        }
        
        // Context menu handler
        private void chooseIndiContextMenuDetails_Click(Object sender, System.EventArgs e) 
        {
            LLClasses.CIndividualRecord ir = null;
            CListableBool li = null;

            if( m_sortableListView.SelectedItems.Count == 1 )
            {
                li = (CListableBool)((ListViewItem)(m_sortableListView.SelectedItems[0]));
                ir = (LLClasses.CIndividualRecord)li.ISRecord;
            }

            m_mainForm.ShowIndividualDetailsDialog( this, li, ir, false, false );
        }

        // Context menu opening handler
        protected void chooseIndiContextMenu_popup(System.Object sender, System.EventArgs e)
        {
            int nSelected = m_sortableListView.SelectedItems.Count;
            m_menuitemChooseIndiMenuDetails.Enabled = (nSelected==1);
        }

        // Returns a list of all selected individuals
        public ArrayList SelectedIndividuals
        {
            get
            {
                ArrayList indis = new ArrayList();
                foreach( ListViewItem li in m_sortableListView.SelectedItems )
                {
                    if( li is CListableBool )
                    {
                        indis.Add( (((CListableBool)((ListViewItem)li)).ISRecord).m_xref );
                    }
                }
                return indis;
            }
        }
    }
}

