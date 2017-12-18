namespace GKUI.Forms
{
    partial class PersonalNameEditDlg
    {
        private System.Windows.Forms.TextBox txtMarriedSurname;
        private System.Windows.Forms.Label lblMarriedSurname;
        private System.Windows.Forms.ComboBox cmbNameType;
        private System.Windows.Forms.Label lblType;
        private System.Windows.Forms.TextBox txtNickname;
        private System.Windows.Forms.TextBox txtNameSuffix;
        private System.Windows.Forms.TextBox txtNamePrefix;
        private System.Windows.Forms.TextBox txtSurnamePrefix;
        private System.Windows.Forms.TextBox txtPatronymic;
        private System.Windows.Forms.TextBox txtName;
        private System.Windows.Forms.TextBox txtSurname;
        private System.Windows.Forms.Label lblNickname;
        private System.Windows.Forms.Label lblNameSuffix;
        private System.Windows.Forms.Label lblNamePrefix;
        private System.Windows.Forms.Label lblSurnamePrefix;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Label lblPatronymic;
        private System.Windows.Forms.Label lblName;
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Label lblSurname;
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.ComboBox cmbLanguage;
        private System.Windows.Forms.Label lblLanguage;

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.lblSurname = new System.Windows.Forms.Label();
            this.btnAccept = new System.Windows.Forms.Button();
            this.lblName = new System.Windows.Forms.Label();
            this.lblPatronymic = new System.Windows.Forms.Label();
            this.btnCancel = new System.Windows.Forms.Button();
            this.lblSurnamePrefix = new System.Windows.Forms.Label();
            this.lblNamePrefix = new System.Windows.Forms.Label();
            this.lblNameSuffix = new System.Windows.Forms.Label();
            this.lblNickname = new System.Windows.Forms.Label();
            this.txtSurname = new System.Windows.Forms.TextBox();
            this.txtName = new System.Windows.Forms.TextBox();
            this.txtPatronymic = new System.Windows.Forms.TextBox();
            this.txtSurnamePrefix = new System.Windows.Forms.TextBox();
            this.txtNamePrefix = new System.Windows.Forms.TextBox();
            this.txtNameSuffix = new System.Windows.Forms.TextBox();
            this.txtNickname = new System.Windows.Forms.TextBox();
            this.lblType = new System.Windows.Forms.Label();
            this.cmbNameType = new System.Windows.Forms.ComboBox();
            this.lblMarriedSurname = new System.Windows.Forms.Label();
            this.txtMarriedSurname = new System.Windows.Forms.TextBox();
            this.cmbLanguage = new System.Windows.Forms.ComboBox();
            this.lblLanguage = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // lblSurname
            // 
            this.lblSurname.AutoSize = true;
            this.lblSurname.Location = new System.Drawing.Point(12, 9);
            this.lblSurname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblSurname.Name = "lblSurname";
            this.lblSurname.Size = new System.Drawing.Size(75, 17);
            this.lblSurname.TabIndex = 0;
            this.lblSurname.Text = "lblSurname";
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(200, 333);
            this.btnAccept.Margin = new System.Windows.Forms.Padding(2);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(114, 30);
            this.btnAccept.TabIndex = 18;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
            // 
            // lblName
            // 
            this.lblName.AutoSize = true;
            this.lblName.Location = new System.Drawing.Point(12, 108);
            this.lblName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblName.Name = "lblName";
            this.lblName.Size = new System.Drawing.Size(55, 17);
            this.lblName.TabIndex = 4;
            this.lblName.Text = "lblName";
            // 
            // lblPatronymic
            // 
            this.lblPatronymic.AutoSize = true;
            this.lblPatronymic.Location = new System.Drawing.Point(12, 155);
            this.lblPatronymic.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblPatronymic.Name = "lblPatronymic";
            this.lblPatronymic.Size = new System.Drawing.Size(90, 17);
            this.lblPatronymic.TabIndex = 6;
            this.lblPatronymic.Text = "lblPatronymic";
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(319, 333);
            this.btnCancel.Margin = new System.Windows.Forms.Padding(2);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(114, 30);
            this.btnCancel.TabIndex = 19;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // lblSurnamePrefix
            // 
            this.lblSurnamePrefix.AutoSize = true;
            this.lblSurnamePrefix.Location = new System.Drawing.Point(259, 9);
            this.lblSurnamePrefix.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblSurnamePrefix.Name = "lblSurnamePrefix";
            this.lblSurnamePrefix.Size = new System.Drawing.Size(109, 17);
            this.lblSurnamePrefix.TabIndex = 8;
            this.lblSurnamePrefix.Text = "lblSurnamePrefix";
            // 
            // lblNamePrefix
            // 
            this.lblNamePrefix.AutoSize = true;
            this.lblNamePrefix.Location = new System.Drawing.Point(259, 58);
            this.lblNamePrefix.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblNamePrefix.Name = "lblNamePrefix";
            this.lblNamePrefix.Size = new System.Drawing.Size(89, 17);
            this.lblNamePrefix.TabIndex = 10;
            this.lblNamePrefix.Text = "lblNamePrefix";
            // 
            // lblNameSuffix
            // 
            this.lblNameSuffix.AutoSize = true;
            this.lblNameSuffix.Location = new System.Drawing.Point(259, 108);
            this.lblNameSuffix.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblNameSuffix.Name = "lblNameSuffix";
            this.lblNameSuffix.Size = new System.Drawing.Size(89, 17);
            this.lblNameSuffix.TabIndex = 12;
            this.lblNameSuffix.Text = "lblNameSuffix";
            // 
            // lblNickname
            // 
            this.lblNickname.AutoSize = true;
            this.lblNickname.Location = new System.Drawing.Point(259, 155);
            this.lblNickname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblNickname.Name = "lblNickname";
            this.lblNickname.Size = new System.Drawing.Size(79, 17);
            this.lblNickname.TabIndex = 14;
            this.lblNickname.Text = "lblNickname";
            // 
            // txtSurname
            // 
            this.txtSurname.Location = new System.Drawing.Point(12, 29);
            this.txtSurname.Margin = new System.Windows.Forms.Padding(2);
            this.txtSurname.Name = "txtSurname";
            this.txtSurname.Size = new System.Drawing.Size(226, 24);
            this.txtSurname.TabIndex = 1;
            // 
            // txtName
            // 
            this.txtName.Location = new System.Drawing.Point(12, 126);
            this.txtName.Margin = new System.Windows.Forms.Padding(2);
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(226, 24);
            this.txtName.TabIndex = 5;
            // 
            // txtPatronymic
            // 
            this.txtPatronymic.Location = new System.Drawing.Point(12, 175);
            this.txtPatronymic.Margin = new System.Windows.Forms.Padding(2);
            this.txtPatronymic.Name = "txtPatronymic";
            this.txtPatronymic.Size = new System.Drawing.Size(226, 24);
            this.txtPatronymic.TabIndex = 7;
            // 
            // txtSurnamePrefix
            // 
            this.txtSurnamePrefix.Location = new System.Drawing.Point(259, 29);
            this.txtSurnamePrefix.Margin = new System.Windows.Forms.Padding(2);
            this.txtSurnamePrefix.Name = "txtSurnamePrefix";
            this.txtSurnamePrefix.Size = new System.Drawing.Size(169, 24);
            this.txtSurnamePrefix.TabIndex = 9;
            // 
            // txtNamePrefix
            // 
            this.txtNamePrefix.Location = new System.Drawing.Point(259, 78);
            this.txtNamePrefix.Margin = new System.Windows.Forms.Padding(2);
            this.txtNamePrefix.Name = "txtNamePrefix";
            this.txtNamePrefix.Size = new System.Drawing.Size(169, 24);
            this.txtNamePrefix.TabIndex = 11;
            // 
            // txtNameSuffix
            // 
            this.txtNameSuffix.Location = new System.Drawing.Point(259, 126);
            this.txtNameSuffix.Margin = new System.Windows.Forms.Padding(2);
            this.txtNameSuffix.Name = "txtNameSuffix";
            this.txtNameSuffix.Size = new System.Drawing.Size(169, 24);
            this.txtNameSuffix.TabIndex = 13;
            // 
            // txtNickname
            // 
            this.txtNickname.Location = new System.Drawing.Point(259, 175);
            this.txtNickname.Margin = new System.Windows.Forms.Padding(2);
            this.txtNickname.Name = "txtNickname";
            this.txtNickname.Size = new System.Drawing.Size(169, 24);
            this.txtNickname.TabIndex = 15;
            // 
            // lblType
            // 
            this.lblType.AutoSize = true;
            this.lblType.Location = new System.Drawing.Point(12, 210);
            this.lblType.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblType.Name = "lblType";
            this.lblType.Size = new System.Drawing.Size(51, 17);
            this.lblType.TabIndex = 16;
            this.lblType.Text = "lblType";
            // 
            // cmbNameType
            // 
            this.cmbNameType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbNameType.Location = new System.Drawing.Point(12, 229);
            this.cmbNameType.Margin = new System.Windows.Forms.Padding(2);
            this.cmbNameType.Name = "cmbNameType";
            this.cmbNameType.Size = new System.Drawing.Size(225, 25);
            this.cmbNameType.TabIndex = 17;
            // 
            // lblMarriedSurname
            // 
            this.lblMarriedSurname.AutoSize = true;
            this.lblMarriedSurname.Location = new System.Drawing.Point(12, 58);
            this.lblMarriedSurname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblMarriedSurname.Name = "lblMarriedSurname";
            this.lblMarriedSurname.Size = new System.Drawing.Size(119, 17);
            this.lblMarriedSurname.TabIndex = 2;
            this.lblMarriedSurname.Text = "lblMarriedSurname";
            // 
            // txtMarriedSurname
            // 
            this.txtMarriedSurname.Location = new System.Drawing.Point(12, 78);
            this.txtMarriedSurname.Margin = new System.Windows.Forms.Padding(2);
            this.txtMarriedSurname.Name = "txtMarriedSurname";
            this.txtMarriedSurname.Size = new System.Drawing.Size(226, 24);
            this.txtMarriedSurname.TabIndex = 3;
            // 
            // cmbLanguage
            // 
            this.cmbLanguage.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbLanguage.Location = new System.Drawing.Point(12, 285);
            this.cmbLanguage.Margin = new System.Windows.Forms.Padding(4);
            this.cmbLanguage.Name = "cmbLanguage";
            this.cmbLanguage.Size = new System.Drawing.Size(226, 25);
            this.cmbLanguage.Sorted = true;
            this.cmbLanguage.TabIndex = 21;
            // 
            // lblLanguage
            // 
            this.lblLanguage.AutoSize = true;
            this.lblLanguage.Location = new System.Drawing.Point(12, 264);
            this.lblLanguage.Margin = new System.Windows.Forms.Padding(8, 8, 0, 0);
            this.lblLanguage.Name = "lblLanguage";
            this.lblLanguage.Size = new System.Drawing.Size(80, 17);
            this.lblLanguage.TabIndex = 20;
            this.lblLanguage.Text = "lblLanguage";
            // 
            // PersonalNameEditDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(444, 374);
            this.Controls.Add(this.cmbLanguage);
            this.Controls.Add(this.lblLanguage);
            this.Controls.Add(this.lblMarriedSurname);
            this.Controls.Add(this.txtMarriedSurname);
            this.Controls.Add(this.lblType);
            this.Controls.Add(this.cmbNameType);
            this.Controls.Add(this.lblSurname);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.lblName);
            this.Controls.Add(this.lblPatronymic);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.lblSurnamePrefix);
            this.Controls.Add(this.lblNamePrefix);
            this.Controls.Add(this.lblNameSuffix);
            this.Controls.Add(this.lblNickname);
            this.Controls.Add(this.txtSurname);
            this.Controls.Add(this.txtName);
            this.Controls.Add(this.txtPatronymic);
            this.Controls.Add(this.txtSurnamePrefix);
            this.Controls.Add(this.txtNamePrefix);
            this.Controls.Add(this.txtNameSuffix);
            this.Controls.Add(this.txtNickname);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "PersonalNameEditDlg";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "PersonalNameEditDlg";
            this.ResumeLayout(false);
            this.PerformLayout();
        }
    }
}
