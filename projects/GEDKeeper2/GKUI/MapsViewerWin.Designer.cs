using System;

namespace GKUI
{
	partial class MapsViewerWin
	{
		private System.ComponentModel.IContainer components = null;
		private System.Windows.Forms.StatusBar StatusBar1;
		private System.Windows.Forms.TabControl PageControl1;
		private System.Windows.Forms.TabPage pagePlaces;
		private System.Windows.Forms.TreeView tvPlaces;
		private System.Windows.Forms.Panel Panel1;
		private System.Windows.Forms.GroupBox grpSelection;
		private System.Windows.Forms.ComboBox cmbPersons;
		private System.Windows.Forms.CheckBox chkResidence;
		private System.Windows.Forms.CheckBox chkDeath;
		private System.Windows.Forms.CheckBox chkBirth;
		private System.Windows.Forms.Button btnSelectPlaces;
		private System.Windows.Forms.Button btnSaveImage;
		private System.Windows.Forms.RadioButton radTotal;
		private System.Windows.Forms.RadioButton radSelected;
		private System.Windows.Forms.CheckBox chkLinesVisible;

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.fPlaces.Dispose();
				this.fMapPoints.Dispose();
				if (components != null) {
					components.Dispose();
				}
			}
			base.Dispose(Disposing);
		}

		private void InitializeComponent()
		{
			this.StatusBar1 = new System.Windows.Forms.StatusBar();
			this.PageControl1 = new System.Windows.Forms.TabControl();
			this.pagePlaces = new System.Windows.Forms.TabPage();
			this.tvPlaces = new System.Windows.Forms.TreeView();
			this.grpSelection = new System.Windows.Forms.GroupBox();
			this.cmbPersons = new System.Windows.Forms.ComboBox();
			this.chkResidence = new System.Windows.Forms.CheckBox();
			this.chkDeath = new System.Windows.Forms.CheckBox();
			this.chkBirth = new System.Windows.Forms.CheckBox();
			this.btnSelectPlaces = new System.Windows.Forms.Button();
			this.btnSaveImage = new System.Windows.Forms.Button();
			this.radTotal = new System.Windows.Forms.RadioButton();
			this.radSelected = new System.Windows.Forms.RadioButton();
			this.chkLinesVisible = new System.Windows.Forms.CheckBox();
			this.Panel1 = new System.Windows.Forms.Panel();
			this.PageControl1.SuspendLayout();
			this.pagePlaces.SuspendLayout();
			this.grpSelection.SuspendLayout();
			this.SuspendLayout();
			// 
			// StatusBar1
			// 
			this.StatusBar1.Location = new System.Drawing.Point(0, 549);
			this.StatusBar1.Name = "StatusBar1";
			this.StatusBar1.Size = new System.Drawing.Size(1101, 23);
			this.StatusBar1.TabIndex = 3;
			// 
			// PageControl1
			// 
			this.PageControl1.Controls.Add(this.pagePlaces);
			this.PageControl1.Dock = System.Windows.Forms.DockStyle.Left;
			this.PageControl1.Location = new System.Drawing.Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(405, 549);
			this.PageControl1.TabIndex = 1;
			// 
			// pagePlaces
			// 
			this.pagePlaces.Controls.Add(this.tvPlaces);
			this.pagePlaces.Controls.Add(this.grpSelection);
			this.pagePlaces.Location = new System.Drawing.Point(4, 26);
			this.pagePlaces.Name = "pagePlaces";
			this.pagePlaces.Size = new System.Drawing.Size(397, 519);
			this.pagePlaces.TabIndex = 0;
			this.pagePlaces.Text = "pagePlaces";
			// 
			// tvPlaces
			// 
			this.tvPlaces.Dock = System.Windows.Forms.DockStyle.Fill;
			this.tvPlaces.Location = new System.Drawing.Point(0, 225);
			this.tvPlaces.Name = "tvPlaces";
			this.tvPlaces.Size = new System.Drawing.Size(397, 294);
			this.tvPlaces.TabIndex = 0;
			this.tvPlaces.DoubleClick += new System.EventHandler(this.TreePlaces_DoubleClick);
			// 
			// grpSelection
			// 
			this.grpSelection.Controls.Add(this.cmbPersons);
			this.grpSelection.Controls.Add(this.chkResidence);
			this.grpSelection.Controls.Add(this.chkDeath);
			this.grpSelection.Controls.Add(this.chkBirth);
			this.grpSelection.Controls.Add(this.btnSelectPlaces);
			this.grpSelection.Controls.Add(this.btnSaveImage);
			this.grpSelection.Controls.Add(this.radTotal);
			this.grpSelection.Controls.Add(this.radSelected);
			this.grpSelection.Controls.Add(this.chkLinesVisible);
			this.grpSelection.Dock = System.Windows.Forms.DockStyle.Top;
			this.grpSelection.Location = new System.Drawing.Point(0, 0);
			this.grpSelection.Name = "grpSelection";
			this.grpSelection.Size = new System.Drawing.Size(397, 225);
			this.grpSelection.TabIndex = 1;
			this.grpSelection.TabStop = false;
			this.grpSelection.Text = "grpSelection";
			// 
			// cmbPersons
			// 
			this.cmbPersons.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cmbPersons.Location = new System.Drawing.Point(11, 126);
			this.cmbPersons.Name = "cmbPersons";
			this.cmbPersons.Size = new System.Drawing.Size(371, 25);
			this.cmbPersons.TabIndex = 5;
			// 
			// chkResidence
			// 
			this.chkResidence.AutoSize = true;
			this.chkResidence.Location = new System.Drawing.Point(27, 78);
			this.chkResidence.Name = "chkResidence";
			this.chkResidence.Size = new System.Drawing.Size(152, 21);
			this.chkResidence.TabIndex = 3;
			this.chkResidence.Text = "chkResidence";
			// 
			// chkDeath
			// 
			this.chkDeath.AutoSize = true;
			this.chkDeath.Location = new System.Drawing.Point(27, 58);
			this.chkDeath.Name = "chkDeath";
			this.chkDeath.Size = new System.Drawing.Size(120, 21);
			this.chkDeath.TabIndex = 2;
			this.chkDeath.Text = "chkDeath";
			// 
			// chkBirth
			// 
			this.chkBirth.AutoSize = true;
			this.chkBirth.Location = new System.Drawing.Point(27, 39);
			this.chkBirth.Name = "chkBirth";
			this.chkBirth.Size = new System.Drawing.Size(137, 21);
			this.chkBirth.TabIndex = 1;
			this.chkBirth.Text = "chkBirth";
			// 
			// btnSelectPlaces
			// 
			this.btnSelectPlaces.Enabled = false;
			this.btnSelectPlaces.Location = new System.Drawing.Point(277, 185);
			this.btnSelectPlaces.Name = "btnSelectPlaces";
			this.btnSelectPlaces.Size = new System.Drawing.Size(105, 30);
			this.btnSelectPlaces.TabIndex = 6;
			this.btnSelectPlaces.Text = "btnSelectPlaces";
			this.btnSelectPlaces.Click += new System.EventHandler(this.btnSelectPlaces_Click);
			// 
			// btnSaveImage
			// 
			this.btnSaveImage.Location = new System.Drawing.Point(11, 185);
			this.btnSaveImage.Name = "btnSaveImage";
			this.btnSaveImage.Size = new System.Drawing.Size(170, 30);
			this.btnSaveImage.TabIndex = 7;
			this.btnSaveImage.Text = "btnSaveImage";
			this.btnSaveImage.Click += new System.EventHandler(this.btnSaveImage_Click);
			// 
			// radTotal
			// 
			this.radTotal.AutoSize = true;
			this.radTotal.Location = new System.Drawing.Point(11, 19);
			this.radTotal.Name = "radTotal";
			this.radTotal.Size = new System.Drawing.Size(127, 21);
			this.radTotal.TabIndex = 0;
			this.radTotal.Text = "radTotal";
			this.radTotal.Click += new System.EventHandler(this.radTotal_Click);
			// 
			// radSelected
			// 
			this.radSelected.AutoSize = true;
			this.radSelected.Location = new System.Drawing.Point(11, 106);
			this.radSelected.Name = "radSelected";
			this.radSelected.Size = new System.Drawing.Size(180, 21);
			this.radSelected.TabIndex = 4;
			this.radSelected.Text = "radSelected";
			this.radSelected.Click += new System.EventHandler(this.radTotal_Click);
			// 
			// chkLinesVisible
			// 
			this.chkLinesVisible.AutoSize = true;
			this.chkLinesVisible.Checked = true;
			this.chkLinesVisible.CheckState = System.Windows.Forms.CheckState.Checked;
			this.chkLinesVisible.Location = new System.Drawing.Point(11, 155);
			this.chkLinesVisible.Name = "chkLinesVisible";
			this.chkLinesVisible.Size = new System.Drawing.Size(155, 21);
			this.chkLinesVisible.TabIndex = 8;
			this.chkLinesVisible.Text = "chkLinesVisible";
			// 
			// Panel1
			// 
			this.Panel1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.Panel1.Location = new System.Drawing.Point(405, 0);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new System.Drawing.Size(696, 549);
			this.Panel1.TabIndex = 4;
			// 
			// MapsViewerWin
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.ClientSize = new System.Drawing.Size(1101, 572);
			this.Controls.Add(this.Panel1);
			this.Controls.Add(this.PageControl1);
			this.Controls.Add(this.StatusBar1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.KeyPreview = true;
			this.Name = "MapsViewerWin";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "MapsViewerWin";
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TfmMaps_KeyDown);
			this.PageControl1.ResumeLayout(false);
			this.pagePlaces.ResumeLayout(false);
			this.grpSelection.ResumeLayout(false);
			this.grpSelection.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}
