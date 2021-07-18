namespace GKUI.Forms
{
	partial class EventEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.TabControl tabsData;
		private System.Windows.Forms.TabPage pageNotes;
		private System.Windows.Forms.TabPage pageMultimedia;
		private System.Windows.Forms.TabPage pageSources;
		private System.Windows.Forms.Button btnAddress;
		private System.Windows.Forms.TabPage pageCommon;
		private System.Windows.Forms.Label lblEvent;
		private System.Windows.Forms.Label lblPlace;
		private System.Windows.Forms.Label lblCause;
		private System.Windows.Forms.Label lblOrg;
		private System.Windows.Forms.Label lblAttrValue;
		private System.Windows.Forms.ComboBox cmbEventType;
		private System.Windows.Forms.TextBox txtEventName;
		private System.Windows.Forms.TextBox txtEventPlace;
		private System.Windows.Forms.TextBox txtEventCause;
		private System.Windows.Forms.TextBox txtEventOrg;
		private System.Windows.Forms.ComboBox txtAttribute;
		private System.Windows.Forms.Button btnPlaceAdd;
		private System.Windows.Forms.Button btnPlaceDelete;
        private Components.GKDateControl dateCtl;

        private void InitializeComponent()
		{
		    this.btnAccept = new System.Windows.Forms.Button();
		    this.btnCancel = new System.Windows.Forms.Button();
		    this.tabsData = new System.Windows.Forms.TabControl();
		    this.pageCommon = new System.Windows.Forms.TabPage();
		    this.dateCtl = new GKUI.Components.GKDateControl();
		    this.lblEvent = new System.Windows.Forms.Label();
		    this.lblPlace = new System.Windows.Forms.Label();
		    this.lblCause = new System.Windows.Forms.Label();
		    this.lblOrg = new System.Windows.Forms.Label();
		    this.lblAttrValue = new System.Windows.Forms.Label();
		    this.btnPlaceAdd = new System.Windows.Forms.Button();
		    this.btnPlaceDelete = new System.Windows.Forms.Button();
		    this.cmbEventType = new System.Windows.Forms.ComboBox();
		    this.txtEventName = new System.Windows.Forms.TextBox();
		    this.txtEventPlace = new System.Windows.Forms.TextBox();
		    this.txtEventCause = new System.Windows.Forms.TextBox();
		    this.txtEventOrg = new System.Windows.Forms.TextBox();
		    this.txtAttribute = new System.Windows.Forms.ComboBox();
		    this.pageNotes = new System.Windows.Forms.TabPage();
		    this.pageMultimedia = new System.Windows.Forms.TabPage();
		    this.pageSources = new System.Windows.Forms.TabPage();
		    this.btnAddress = new System.Windows.Forms.Button();
		    this.tabsData.SuspendLayout();
		    this.pageCommon.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // btnAccept
		    // 
		    this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnAccept.Location = new System.Drawing.Point(297, 358);
		    this.btnAccept.Name = "btnAccept";
		    this.btnAccept.Size = new System.Drawing.Size(90, 24);
		    this.btnAccept.TabIndex = 2;
		    this.btnAccept.Text = "btnAccept";
		    this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
		    // 
		    // btnCancel
		    // 
		    this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnCancel.Location = new System.Drawing.Point(395, 358);
		    this.btnCancel.Name = "btnCancel";
		    this.btnCancel.Size = new System.Drawing.Size(90, 24);
		    this.btnCancel.TabIndex = 3;
		    this.btnCancel.Text = "btnCancel";
		    this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
		    // 
		    // tabsData
		    // 
		    this.tabsData.Controls.Add(this.pageCommon);
		    this.tabsData.Controls.Add(this.pageNotes);
		    this.tabsData.Controls.Add(this.pageMultimedia);
		    this.tabsData.Controls.Add(this.pageSources);
		    this.tabsData.Dock = System.Windows.Forms.DockStyle.Top;
		    this.tabsData.Location = new System.Drawing.Point(0, 0);
		    this.tabsData.Name = "tabsData";
		    this.tabsData.SelectedIndex = 0;
		    this.tabsData.Size = new System.Drawing.Size(496, 343);
		    this.tabsData.TabIndex = 0;
		    // 
		    // pageCommon
		    // 
		    this.pageCommon.Controls.Add(this.dateCtl);
		    this.pageCommon.Controls.Add(this.lblEvent);
		    this.pageCommon.Controls.Add(this.lblPlace);
		    this.pageCommon.Controls.Add(this.lblCause);
		    this.pageCommon.Controls.Add(this.lblOrg);
		    this.pageCommon.Controls.Add(this.lblAttrValue);
		    this.pageCommon.Controls.Add(this.btnPlaceAdd);
		    this.pageCommon.Controls.Add(this.btnPlaceDelete);
		    this.pageCommon.Controls.Add(this.cmbEventType);
		    this.pageCommon.Controls.Add(this.txtEventName);
		    this.pageCommon.Controls.Add(this.txtEventPlace);
		    this.pageCommon.Controls.Add(this.txtEventCause);
		    this.pageCommon.Controls.Add(this.txtEventOrg);
		    this.pageCommon.Controls.Add(this.txtAttribute);
		    this.pageCommon.Location = new System.Drawing.Point(4, 22);
		    this.pageCommon.Name = "pageCommon";
		    this.pageCommon.Size = new System.Drawing.Size(488, 317);
		    this.pageCommon.TabIndex = 0;
		    this.pageCommon.Text = "pageCommon";
		    // 
		    // dateCtl
		    // 
		    this.dateCtl.Location = new System.Drawing.Point(5, 149);
		    this.dateCtl.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
		    this.dateCtl.Name = "dateCtl";
		    this.dateCtl.Size = new System.Drawing.Size(473, 75);
		    this.dateCtl.TabIndex = 10;
		    // 
		    // lblEvent
		    // 
		    this.lblEvent.AutoSize = true;
		    this.lblEvent.Location = new System.Drawing.Point(9, 8);
		    this.lblEvent.Name = "lblEvent";
		    this.lblEvent.Size = new System.Drawing.Size(52, 13);
		    this.lblEvent.TabIndex = 0;
		    this.lblEvent.Text = "lblEvent";
		    // 
		    // lblPlace
		    // 
		    this.lblPlace.AutoSize = true;
		    this.lblPlace.Location = new System.Drawing.Point(9, 101);
		    this.lblPlace.Name = "lblPlace";
		    this.lblPlace.Size = new System.Drawing.Size(50, 13);
		    this.lblPlace.TabIndex = 5;
		    this.lblPlace.Text = "lblPlace";
		    // 
		    // lblCause
		    // 
		    this.lblCause.AutoSize = true;
		    this.lblCause.Location = new System.Drawing.Point(9, 226);
		    this.lblCause.Name = "lblCause";
		    this.lblCause.Size = new System.Drawing.Size(56, 13);
		    this.lblCause.TabIndex = 18;
		    this.lblCause.Text = "lblCause";
		    // 
		    // lblOrg
		    // 
		    this.lblOrg.AutoSize = true;
		    this.lblOrg.Location = new System.Drawing.Point(9, 272);
		    this.lblOrg.Name = "lblOrg";
		    this.lblOrg.Size = new System.Drawing.Size(41, 13);
		    this.lblOrg.TabIndex = 20;
		    this.lblOrg.Text = "lblOrg";
		    // 
		    // lblAttrValue
		    // 
		    this.lblAttrValue.AutoSize = true;
		    this.lblAttrValue.Location = new System.Drawing.Point(9, 54);
		    this.lblAttrValue.Name = "lblAttrValue";
		    this.lblAttrValue.Size = new System.Drawing.Size(72, 13);
		    this.lblAttrValue.TabIndex = 3;
		    this.lblAttrValue.Text = "lblAttrValue";
		    // 
		    // btnPlaceAdd
		    // 
		    this.btnPlaceAdd.Enabled = false;
		    this.btnPlaceAdd.Location = new System.Drawing.Point(409, 114);
		    this.btnPlaceAdd.Name = "btnPlaceAdd";
		    this.btnPlaceAdd.Size = new System.Drawing.Size(31, 27);
		    this.btnPlaceAdd.TabIndex = 7;
		    this.btnPlaceAdd.Click += new System.EventHandler(this.btnPlaceAdd_Click);
		    // 
		    // btnPlaceDelete
		    // 
		    this.btnPlaceDelete.Enabled = false;
		    this.btnPlaceDelete.Location = new System.Drawing.Point(446, 114);
		    this.btnPlaceDelete.Name = "btnPlaceDelete";
		    this.btnPlaceDelete.Size = new System.Drawing.Size(31, 27);
		    this.btnPlaceDelete.TabIndex = 9;
		    this.btnPlaceDelete.Click += new System.EventHandler(this.btnPlaceDelete_Click);
		    // 
		    // cmbEventType
		    // 
		    this.cmbEventType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
		    this.cmbEventType.Location = new System.Drawing.Point(9, 23);
		    this.cmbEventType.Name = "cmbEventType";
		    this.cmbEventType.Size = new System.Drawing.Size(208, 21);
		    this.cmbEventType.TabIndex = 1;
		    this.cmbEventType.SelectedIndexChanged += new System.EventHandler(this.EditEventType_SelectedIndexChanged);
		    // 
		    // txtEventName
		    // 
		    this.txtEventName.Location = new System.Drawing.Point(224, 23);
		    this.txtEventName.Name = "txtEventName";
		    this.txtEventName.Size = new System.Drawing.Size(254, 21);
		    this.txtEventName.TabIndex = 2;
		    // 
		    // txtEventPlace
		    // 
		    this.txtEventPlace.Location = new System.Drawing.Point(9, 117);
		    this.txtEventPlace.Name = "txtEventPlace";
		    this.txtEventPlace.Size = new System.Drawing.Size(394, 21);
		    this.txtEventPlace.TabIndex = 6;
		    this.txtEventPlace.KeyDown += new System.Windows.Forms.KeyEventHandler(this.EditEventPlace_KeyDown);
		    // 
		    // txtEventCause
		    // 
		    this.txtEventCause.Location = new System.Drawing.Point(9, 241);
		    this.txtEventCause.Name = "txtEventCause";
		    this.txtEventCause.Size = new System.Drawing.Size(469, 21);
		    this.txtEventCause.TabIndex = 19;
		    // 
		    // txtEventOrg
		    // 
		    this.txtEventOrg.Location = new System.Drawing.Point(9, 287);
		    this.txtEventOrg.Name = "txtEventOrg";
		    this.txtEventOrg.Size = new System.Drawing.Size(469, 21);
		    this.txtEventOrg.TabIndex = 21;
		    // 
		    // txtAttribute
		    // 
		    this.txtAttribute.Location = new System.Drawing.Point(9, 70);
		    this.txtAttribute.Name = "txtAttribute";
		    this.txtAttribute.Size = new System.Drawing.Size(469, 21);
		    this.txtAttribute.TabIndex = 4;
		    // 
		    // pageNotes
		    // 
		    this.pageNotes.Location = new System.Drawing.Point(4, 22);
		    this.pageNotes.Name = "pageNotes";
		    this.pageNotes.Size = new System.Drawing.Size(488, 317);
		    this.pageNotes.TabIndex = 1;
		    this.pageNotes.Text = "pageNotes";
		    // 
		    // pageMultimedia
		    // 
		    this.pageMultimedia.Location = new System.Drawing.Point(4, 22);
		    this.pageMultimedia.Name = "pageMultimedia";
		    this.pageMultimedia.Size = new System.Drawing.Size(488, 317);
		    this.pageMultimedia.TabIndex = 2;
		    this.pageMultimedia.Text = "pageMultimedia";
		    // 
		    // pageSources
		    // 
		    this.pageSources.Location = new System.Drawing.Point(4, 22);
		    this.pageSources.Name = "pageSources";
		    this.pageSources.Size = new System.Drawing.Size(488, 317);
		    this.pageSources.TabIndex = 3;
		    this.pageSources.Text = "pageSources";
		    // 
		    // btnAddress
		    // 
		    this.btnAddress.Location = new System.Drawing.Point(9, 358);
		    this.btnAddress.Name = "btnAddress";
		    this.btnAddress.Size = new System.Drawing.Size(91, 24);
		    this.btnAddress.TabIndex = 1;
		    this.btnAddress.Text = "btnAddress";
		    this.btnAddress.Click += new System.EventHandler(this.btnAddress_Click);
		    // 
		    // EventEditDlg
		    // 
		    this.AcceptButton = this.btnAccept;
		    this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnCancel;
		    this.ClientSize = new System.Drawing.Size(496, 394);
		    this.Controls.Add(this.btnAccept);
		    this.Controls.Add(this.btnCancel);
		    this.Controls.Add(this.tabsData);
		    this.Controls.Add(this.btnAddress);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "EventEditDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "EventEditDlg";
		    this.tabsData.ResumeLayout(false);
		    this.pageCommon.ResumeLayout(false);
		    this.pageCommon.PerformLayout();
		    this.ResumeLayout(false);
		}
	}
}
