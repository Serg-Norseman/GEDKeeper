namespace GKLifePlugin.ConwayLife
{
    partial class OptionsForm
    {
        private System.ComponentModel.IContainer components = null;
        
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }
        
        private void InitializeComponent()
        {
            this.tabsMain = new System.Windows.Forms.TabControl();
            this.tabGeneral = new System.Windows.Forms.TabPage();
            this.btnRestoreGeneralDefaults = new System.Windows.Forms.Button();
            this.grpAnimation = new System.Windows.Forms.GroupBox();
            this.edtAnimationDelay = new System.Windows.Forms.NumericUpDown();
            this.lblAnimationDelay = new System.Windows.Forms.Label();
            this.grpColour = new System.Windows.Forms.GroupBox();
            this.lblColourLivingCells = new System.Windows.Forms.Label();
            this.lblColourBackground = new System.Windows.Forms.Label();
            this.grpCellSize = new System.Windows.Forms.GroupBox();
            this.edtGridHeight = new System.Windows.Forms.NumericUpDown();
            this.edtGridWidth = new System.Windows.Forms.NumericUpDown();
            this.lblGridSizeHeight = new System.Windows.Forms.Label();
            this.lblGridSizeWidth = new System.Windows.Forms.Label();
            this.tabRules = new System.Windows.Forms.TabPage();
            this.btnRestoreRuleDefaults = new System.Windows.Forms.Button();
            this.grpDeadCells = new System.Windows.Forms.GroupBox();
            this.chkDeadCell0 = new System.Windows.Forms.CheckBox();
            this.chkDeadCell7 = new System.Windows.Forms.CheckBox();
            this.chkDeadCell8 = new System.Windows.Forms.CheckBox();
            this.chkDeadCell5 = new System.Windows.Forms.CheckBox();
            this.chkDeadCell6 = new System.Windows.Forms.CheckBox();
            this.chkDeadCell3 = new System.Windows.Forms.CheckBox();
            this.chkDeadCell4 = new System.Windows.Forms.CheckBox();
            this.chkDeadCell2 = new System.Windows.Forms.CheckBox();
            this.chkDeadCell1 = new System.Windows.Forms.CheckBox();
            this.grpLiveCells = new System.Windows.Forms.GroupBox();
            this.chkLiveCell7 = new System.Windows.Forms.CheckBox();
            this.chkLiveCell8 = new System.Windows.Forms.CheckBox();
            this.chkLiveCell5 = new System.Windows.Forms.CheckBox();
            this.chkLiveCell6 = new System.Windows.Forms.CheckBox();
            this.chkLiveCell3 = new System.Windows.Forms.CheckBox();
            this.chkLiveCell4 = new System.Windows.Forms.CheckBox();
            this.chkLiveCell2 = new System.Windows.Forms.CheckBox();
            this.chkLiveCell0 = new System.Windows.Forms.CheckBox();
            this.chkLiveCell1 = new System.Windows.Forms.CheckBox();
            this.btnApply = new System.Windows.Forms.Button();
            this.btnClose = new System.Windows.Forms.Button();
            this.colorDialog1 = new System.Windows.Forms.ColorDialog();
            this.tabsMain.SuspendLayout();
            this.tabGeneral.SuspendLayout();
            this.grpAnimation.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.edtAnimationDelay)).BeginInit();
            this.grpColour.SuspendLayout();
            this.grpCellSize.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.edtGridHeight)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.edtGridWidth)).BeginInit();
            this.tabRules.SuspendLayout();
            this.grpDeadCells.SuspendLayout();
            this.grpLiveCells.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabsMain
            // 
            this.tabsMain.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                                                                         | System.Windows.Forms.AnchorStyles.Right)));
            this.tabsMain.Controls.Add(this.tabGeneral);
            this.tabsMain.Controls.Add(this.tabRules);
            this.tabsMain.Location = new System.Drawing.Point(12, 12);
            this.tabsMain.Name = "tabsMain";
            this.tabsMain.SelectedIndex = 0;
            this.tabsMain.Size = new System.Drawing.Size(918, 387);
            this.tabsMain.TabIndex = 0;
            // 
            // tabGeneral
            // 
            this.tabGeneral.Controls.Add(this.btnRestoreGeneralDefaults);
            this.tabGeneral.Controls.Add(this.grpAnimation);
            this.tabGeneral.Controls.Add(this.grpColour);
            this.tabGeneral.Controls.Add(this.grpCellSize);
            this.tabGeneral.Location = new System.Drawing.Point(4, 25);
            this.tabGeneral.Name = "tabGeneral";
            this.tabGeneral.Padding = new System.Windows.Forms.Padding(3);
            this.tabGeneral.Size = new System.Drawing.Size(910, 358);
            this.tabGeneral.TabIndex = 0;
            this.tabGeneral.Text = "Общее";
            this.tabGeneral.UseVisualStyleBackColor = true;
            // 
            // btnRestoreGeneralDefaults
            // 
            this.btnRestoreGeneralDefaults.Location = new System.Drawing.Point(703, 309);
            this.btnRestoreGeneralDefaults.Name = "btnRestoreGeneralDefaults";
            this.btnRestoreGeneralDefaults.Size = new System.Drawing.Size(192, 32);
            this.btnRestoreGeneralDefaults.TabIndex = 3;
            this.btnRestoreGeneralDefaults.Text = "Восстановить обычные";
            this.btnRestoreGeneralDefaults.UseVisualStyleBackColor = true;
            this.btnRestoreGeneralDefaults.Click += new System.EventHandler(this.btnRestoreGeneralDefaults_Click);
            // 
            // grpAnimation
            // 
            this.grpAnimation.Controls.Add(this.edtAnimationDelay);
            this.grpAnimation.Controls.Add(this.lblAnimationDelay);
            this.grpAnimation.Location = new System.Drawing.Point(299, 6);
            this.grpAnimation.Name = "grpAnimation";
            this.grpAnimation.Size = new System.Drawing.Size(304, 181);
            this.grpAnimation.TabIndex = 2;
            this.grpAnimation.TabStop = false;
            this.grpAnimation.Tag = "";
            this.grpAnimation.Text = "Анимация";
            // 
            // edtAnimationDelay
            // 
            this.edtAnimationDelay.Location = new System.Drawing.Point(172, 16);
            this.edtAnimationDelay.Maximum = new decimal(new int[] {
                                                             10000,
                                                             0,
                                                             0,
                                                             0});
            this.edtAnimationDelay.Minimum = new decimal(new int[] {
                                                             50,
                                                             0,
                                                             0,
                                                             0});
            this.edtAnimationDelay.Name = "edtAnimationDelay";
            this.edtAnimationDelay.Size = new System.Drawing.Size(120, 22);
            this.edtAnimationDelay.TabIndex = 1;
            this.edtAnimationDelay.Value = new decimal(new int[] {
                                                           50,
                                                           0,
                                                           0,
                                                           0});
            // 
            // lblAnimationDelay
            // 
            this.lblAnimationDelay.AutoSize = true;
            this.lblAnimationDelay.Location = new System.Drawing.Point(6, 18);
            this.lblAnimationDelay.Name = "lblAnimationDelay";
            this.lblAnimationDelay.Size = new System.Drawing.Size(142, 17);
            this.lblAnimationDelay.TabIndex = 0;
            this.lblAnimationDelay.Text = "Задержка анимации";
            // 
            // grpColour
            // 
            this.grpColour.Controls.Add(this.lblColourLivingCells);
            this.grpColour.Controls.Add(this.lblColourBackground);
            this.grpColour.Location = new System.Drawing.Point(6, 92);
            this.grpColour.Name = "grpColour";
            this.grpColour.Size = new System.Drawing.Size(287, 95);
            this.grpColour.TabIndex = 1;
            this.grpColour.TabStop = false;
            this.grpColour.Text = "Цвета";
            // 
            // lblColourLivingCells
            // 
            this.lblColourLivingCells.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.lblColourLivingCells.Cursor = System.Windows.Forms.Cursors.Hand;
            this.lblColourLivingCells.Location = new System.Drawing.Point(6, 53);
            this.lblColourLivingCells.Name = "lblColourLivingCells";
            this.lblColourLivingCells.Size = new System.Drawing.Size(267, 26);
            this.lblColourLivingCells.TabIndex = 1;
            this.lblColourLivingCells.Text = "Цвет живых ячеек";
            this.lblColourLivingCells.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.lblColourLivingCells.Click += new System.EventHandler(this.lblColours_Click);
            // 
            // lblColourBackground
            // 
            this.lblColourBackground.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.lblColourBackground.Cursor = System.Windows.Forms.Cursors.Hand;
            this.lblColourBackground.Location = new System.Drawing.Point(6, 18);
            this.lblColourBackground.Name = "lblColourBackground";
            this.lblColourBackground.Size = new System.Drawing.Size(267, 26);
            this.lblColourBackground.TabIndex = 0;
            this.lblColourBackground.Text = "Цвет фона";
            this.lblColourBackground.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.lblColourBackground.Click += new System.EventHandler(this.lblColours_Click);
            // 
            // grpCellSize
            // 
            this.grpCellSize.Controls.Add(this.edtGridHeight);
            this.grpCellSize.Controls.Add(this.edtGridWidth);
            this.grpCellSize.Controls.Add(this.lblGridSizeHeight);
            this.grpCellSize.Controls.Add(this.lblGridSizeWidth);
            this.grpCellSize.Location = new System.Drawing.Point(6, 6);
            this.grpCellSize.Name = "grpCellSize";
            this.grpCellSize.Size = new System.Drawing.Size(287, 80);
            this.grpCellSize.TabIndex = 0;
            this.grpCellSize.TabStop = false;
            this.grpCellSize.Text = "Поле";
            // 
            // edtGridHeight
            // 
            this.edtGridHeight.Location = new System.Drawing.Point(153, 44);
            this.edtGridHeight.Maximum = new decimal(new int[] {
                                                         1000,
                                                         0,
                                                         0,
                                                         0});
            this.edtGridHeight.Minimum = new decimal(new int[] {
                                                         5,
                                                         0,
                                                         0,
                                                         0});
            this.edtGridHeight.Name = "edtGridHeight";
            this.edtGridHeight.Size = new System.Drawing.Size(120, 22);
            this.edtGridHeight.TabIndex = 3;
            this.edtGridHeight.Value = new decimal(new int[] {
                                                       5,
                                                       0,
                                                       0,
                                                       0});
            // 
            // edtGridWidth
            // 
            this.edtGridWidth.Location = new System.Drawing.Point(153, 16);
            this.edtGridWidth.Maximum = new decimal(new int[] {
                                                        1000,
                                                        0,
                                                        0,
                                                        0});
            this.edtGridWidth.Minimum = new decimal(new int[] {
                                                        5,
                                                        0,
                                                        0,
                                                        0});
            this.edtGridWidth.Name = "edtGridWidth";
            this.edtGridWidth.Size = new System.Drawing.Size(120, 22);
            this.edtGridWidth.TabIndex = 2;
            this.edtGridWidth.Value = new decimal(new int[] {
                                                      5,
                                                      0,
                                                      0,
                                                      0});
            // 
            // lblGridSizeHeight
            // 
            this.lblGridSizeHeight.Location = new System.Drawing.Point(6, 46);
            this.lblGridSizeHeight.Name = "lblGridSizeHeight";
            this.lblGridSizeHeight.Size = new System.Drawing.Size(135, 23);
            this.lblGridSizeHeight.TabIndex = 1;
            this.lblGridSizeHeight.Text = "Высота поля";
            // 
            // lblGridSizeWidth
            // 
            this.lblGridSizeWidth.Location = new System.Drawing.Point(6, 18);
            this.lblGridSizeWidth.Name = "lblGridSizeWidth";
            this.lblGridSizeWidth.Size = new System.Drawing.Size(141, 23);
            this.lblGridSizeWidth.TabIndex = 0;
            this.lblGridSizeWidth.Text = "Ширина поля";
            // 
            // tabRules
            // 
            this.tabRules.Controls.Add(this.btnRestoreRuleDefaults);
            this.tabRules.Controls.Add(this.grpDeadCells);
            this.tabRules.Controls.Add(this.grpLiveCells);
            this.tabRules.Location = new System.Drawing.Point(4, 25);
            this.tabRules.Name = "tabRules";
            this.tabRules.Padding = new System.Windows.Forms.Padding(3);
            this.tabRules.Size = new System.Drawing.Size(910, 358);
            this.tabRules.TabIndex = 1;
            this.tabRules.Text = "Правила";
            this.tabRules.UseVisualStyleBackColor = true;
            // 
            // btnRestoreRuleDefaults
            // 
            this.btnRestoreRuleDefaults.Location = new System.Drawing.Point(700, 309);
            this.btnRestoreRuleDefaults.Name = "btnRestoreRuleDefaults";
            this.btnRestoreRuleDefaults.Size = new System.Drawing.Size(192, 32);
            this.btnRestoreRuleDefaults.TabIndex = 4;
            this.btnRestoreRuleDefaults.Text = "Восстановить обычные";
            this.btnRestoreRuleDefaults.UseVisualStyleBackColor = true;
            this.btnRestoreRuleDefaults.Click += new System.EventHandler(this.btnRestoreRuleDefaults_Click);
            // 
            // grpDeadCells
            // 
            this.grpDeadCells.Controls.Add(this.chkDeadCell0);
            this.grpDeadCells.Controls.Add(this.chkDeadCell7);
            this.grpDeadCells.Controls.Add(this.chkDeadCell8);
            this.grpDeadCells.Controls.Add(this.chkDeadCell5);
            this.grpDeadCells.Controls.Add(this.chkDeadCell6);
            this.grpDeadCells.Controls.Add(this.chkDeadCell3);
            this.grpDeadCells.Controls.Add(this.chkDeadCell4);
            this.grpDeadCells.Controls.Add(this.chkDeadCell2);
            this.grpDeadCells.Controls.Add(this.chkDeadCell1);
            this.grpDeadCells.Location = new System.Drawing.Point(18, 139);
            this.grpDeadCells.Name = "grpDeadCells";
            this.grpDeadCells.Size = new System.Drawing.Size(560, 109);
            this.grpDeadCells.TabIndex = 1;
            this.grpDeadCells.TabStop = false;
            this.grpDeadCells.Text = "Ячейки мертвы, если";
            // 
            // chkDeadCell0
            // 
            this.chkDeadCell0.Location = new System.Drawing.Point(6, 19);
            this.chkDeadCell0.Name = "chkDeadCell0";
            this.chkDeadCell0.Size = new System.Drawing.Size(130, 24);
            this.chkDeadCell0.TabIndex = 16;
            this.chkDeadCell0.Text = "Соседей = 0";
            this.chkDeadCell0.UseVisualStyleBackColor = true;
            // 
            // chkDeadCell7
            // 
            this.chkDeadCell7.Location = new System.Drawing.Point(278, 79);
            this.chkDeadCell7.Name = "chkDeadCell7";
            this.chkDeadCell7.Size = new System.Drawing.Size(130, 24);
            this.chkDeadCell7.TabIndex = 15;
            this.chkDeadCell7.Text = "Соседей = 7";
            this.chkDeadCell7.UseVisualStyleBackColor = true;
            // 
            // chkDeadCell8
            // 
            this.chkDeadCell8.Location = new System.Drawing.Point(414, 79);
            this.chkDeadCell8.Name = "chkDeadCell8";
            this.chkDeadCell8.Size = new System.Drawing.Size(130, 24);
            this.chkDeadCell8.TabIndex = 14;
            this.chkDeadCell8.Text = "Соседей = 8";
            this.chkDeadCell8.UseVisualStyleBackColor = true;
            // 
            // chkDeadCell5
            // 
            this.chkDeadCell5.Location = new System.Drawing.Point(6, 79);
            this.chkDeadCell5.Name = "chkDeadCell5";
            this.chkDeadCell5.Size = new System.Drawing.Size(130, 24);
            this.chkDeadCell5.TabIndex = 13;
            this.chkDeadCell5.Text = "Соседей = 5";
            this.chkDeadCell5.UseVisualStyleBackColor = true;
            // 
            // chkDeadCell6
            // 
            this.chkDeadCell6.Location = new System.Drawing.Point(142, 79);
            this.chkDeadCell6.Name = "chkDeadCell6";
            this.chkDeadCell6.Size = new System.Drawing.Size(130, 24);
            this.chkDeadCell6.TabIndex = 12;
            this.chkDeadCell6.Text = "Соседей = 6";
            this.chkDeadCell6.UseVisualStyleBackColor = true;
            // 
            // chkDeadCell3
            // 
            this.chkDeadCell3.Location = new System.Drawing.Point(278, 49);
            this.chkDeadCell3.Name = "chkDeadCell3";
            this.chkDeadCell3.Size = new System.Drawing.Size(130, 24);
            this.chkDeadCell3.TabIndex = 11;
            this.chkDeadCell3.Text = "Соседей = 3";
            this.chkDeadCell3.UseVisualStyleBackColor = true;
            // 
            // chkDeadCell4
            // 
            this.chkDeadCell4.Location = new System.Drawing.Point(414, 49);
            this.chkDeadCell4.Name = "chkDeadCell4";
            this.chkDeadCell4.Size = new System.Drawing.Size(130, 24);
            this.chkDeadCell4.TabIndex = 10;
            this.chkDeadCell4.Text = "Соседей = 4";
            this.chkDeadCell4.UseVisualStyleBackColor = true;
            // 
            // chkDeadCell2
            // 
            this.chkDeadCell2.Location = new System.Drawing.Point(142, 49);
            this.chkDeadCell2.Name = "chkDeadCell2";
            this.chkDeadCell2.Size = new System.Drawing.Size(130, 24);
            this.chkDeadCell2.TabIndex = 9;
            this.chkDeadCell2.Text = "Соседей = 2";
            this.chkDeadCell2.UseVisualStyleBackColor = true;
            // 
            // chkDeadCell1
            // 
            this.chkDeadCell1.Location = new System.Drawing.Point(6, 49);
            this.chkDeadCell1.Name = "chkDeadCell1";
            this.chkDeadCell1.Size = new System.Drawing.Size(130, 24);
            this.chkDeadCell1.TabIndex = 8;
            this.chkDeadCell1.Text = "Соседей = 1";
            this.chkDeadCell1.UseVisualStyleBackColor = true;
            // 
            // grpLiveCells
            // 
            this.grpLiveCells.Controls.Add(this.chkLiveCell7);
            this.grpLiveCells.Controls.Add(this.chkLiveCell8);
            this.grpLiveCells.Controls.Add(this.chkLiveCell5);
            this.grpLiveCells.Controls.Add(this.chkLiveCell6);
            this.grpLiveCells.Controls.Add(this.chkLiveCell3);
            this.grpLiveCells.Controls.Add(this.chkLiveCell4);
            this.grpLiveCells.Controls.Add(this.chkLiveCell2);
            this.grpLiveCells.Controls.Add(this.chkLiveCell0);
            this.grpLiveCells.Controls.Add(this.chkLiveCell1);
            this.grpLiveCells.Location = new System.Drawing.Point(18, 17);
            this.grpLiveCells.Name = "grpLiveCells";
            this.grpLiveCells.Size = new System.Drawing.Size(560, 116);
            this.grpLiveCells.TabIndex = 0;
            this.grpLiveCells.TabStop = false;
            this.grpLiveCells.Text = "Ячейки живы, если";
            // 
            // chkLiveCell7
            // 
            this.chkLiveCell7.Location = new System.Drawing.Point(278, 81);
            this.chkLiveCell7.Name = "chkLiveCell7";
            this.chkLiveCell7.Size = new System.Drawing.Size(130, 24);
            this.chkLiveCell7.TabIndex = 7;
            this.chkLiveCell7.Text = "Соседей = 7";
            this.chkLiveCell7.UseVisualStyleBackColor = true;
            // 
            // chkLiveCell8
            // 
            this.chkLiveCell8.Location = new System.Drawing.Point(414, 81);
            this.chkLiveCell8.Name = "chkLiveCell8";
            this.chkLiveCell8.Size = new System.Drawing.Size(130, 24);
            this.chkLiveCell8.TabIndex = 6;
            this.chkLiveCell8.Text = "Соседей = 8";
            this.chkLiveCell8.UseVisualStyleBackColor = true;
            // 
            // chkLiveCell5
            // 
            this.chkLiveCell5.Location = new System.Drawing.Point(6, 81);
            this.chkLiveCell5.Name = "chkLiveCell5";
            this.chkLiveCell5.Size = new System.Drawing.Size(130, 24);
            this.chkLiveCell5.TabIndex = 5;
            this.chkLiveCell5.Text = "Соседей = 5";
            this.chkLiveCell5.UseVisualStyleBackColor = true;
            // 
            // chkLiveCell6
            // 
            this.chkLiveCell6.Location = new System.Drawing.Point(142, 81);
            this.chkLiveCell6.Name = "chkLiveCell6";
            this.chkLiveCell6.Size = new System.Drawing.Size(130, 24);
            this.chkLiveCell6.TabIndex = 4;
            this.chkLiveCell6.Text = "Соседей = 6";
            this.chkLiveCell6.UseVisualStyleBackColor = true;
            // 
            // chkLiveCell3
            // 
            this.chkLiveCell3.Location = new System.Drawing.Point(278, 51);
            this.chkLiveCell3.Name = "chkLiveCell3";
            this.chkLiveCell3.Size = new System.Drawing.Size(130, 24);
            this.chkLiveCell3.TabIndex = 3;
            this.chkLiveCell3.Text = "Соседей = 3";
            this.chkLiveCell3.UseVisualStyleBackColor = true;
            // 
            // chkLiveCell4
            // 
            this.chkLiveCell4.Location = new System.Drawing.Point(414, 51);
            this.chkLiveCell4.Name = "chkLiveCell4";
            this.chkLiveCell4.Size = new System.Drawing.Size(130, 24);
            this.chkLiveCell4.TabIndex = 2;
            this.chkLiveCell4.Text = "Соседей = 4";
            this.chkLiveCell4.UseVisualStyleBackColor = true;
            // 
            // chkLiveCell2
            // 
            this.chkLiveCell2.Location = new System.Drawing.Point(142, 51);
            this.chkLiveCell2.Name = "chkLiveCell2";
            this.chkLiveCell2.Size = new System.Drawing.Size(130, 24);
            this.chkLiveCell2.TabIndex = 1;
            this.chkLiveCell2.Text = "Соседей = 2";
            this.chkLiveCell2.UseVisualStyleBackColor = true;
            // 
            // chkLiveCell0
            // 
            this.chkLiveCell0.Location = new System.Drawing.Point(6, 21);
            this.chkLiveCell0.Name = "chkLiveCell0";
            this.chkLiveCell0.Size = new System.Drawing.Size(130, 24);
            this.chkLiveCell0.TabIndex = 0;
            this.chkLiveCell0.Text = "Соседей = 0";
            this.chkLiveCell0.UseVisualStyleBackColor = true;
            // 
            // chkLiveCell1
            // 
            this.chkLiveCell1.Location = new System.Drawing.Point(6, 51);
            this.chkLiveCell1.Name = "chkLiveCell1";
            this.chkLiveCell1.Size = new System.Drawing.Size(130, 24);
            this.chkLiveCell1.TabIndex = 0;
            this.chkLiveCell1.Text = "Соседей = 1";
            this.chkLiveCell1.UseVisualStyleBackColor = true;
            // 
            // btnApply
            // 
            this.btnApply.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.btnApply.Location = new System.Drawing.Point(728, 405);
            this.btnApply.Name = "btnApply";
            this.btnApply.Size = new System.Drawing.Size(96, 32);
            this.btnApply.TabIndex = 1;
            this.btnApply.Text = "Применить";
            this.btnApply.UseVisualStyleBackColor = true;
            this.btnApply.Click += new System.EventHandler(this.btnApply_Click);
            // 
            // btnClose
            // 
            this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnClose.Location = new System.Drawing.Point(830, 405);
            this.btnClose.Name = "btnClose";
            this.btnClose.Size = new System.Drawing.Size(96, 32);
            this.btnClose.TabIndex = 2;
            this.btnClose.Text = "Закрыть";
            this.btnClose.UseVisualStyleBackColor = true;
            // 
            // OptionsForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(942, 449);
            this.Controls.Add(this.btnClose);
            this.Controls.Add(this.btnApply);
            this.Controls.Add(this.tabsMain);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "OptionsForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Опции";
            this.Load += new System.EventHandler(this.OptionsForm_Load);
            this.tabsMain.ResumeLayout(false);
            this.tabGeneral.ResumeLayout(false);
            this.grpAnimation.ResumeLayout(false);
            this.grpAnimation.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.edtAnimationDelay)).EndInit();
            this.grpColour.ResumeLayout(false);
            this.grpCellSize.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.edtGridHeight)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.edtGridWidth)).EndInit();
            this.tabRules.ResumeLayout(false);
            this.grpDeadCells.ResumeLayout(false);
            this.grpLiveCells.ResumeLayout(false);
            this.ResumeLayout(false);
        }
        private System.Windows.Forms.ColorDialog colorDialog1;
        private System.Windows.Forms.CheckBox chkLiveCell0;
        private System.Windows.Forms.CheckBox chkDeadCell0;
        private System.Windows.Forms.CheckBox chkDeadCell1;
        private System.Windows.Forms.CheckBox chkDeadCell2;
        private System.Windows.Forms.CheckBox chkDeadCell4;
        private System.Windows.Forms.CheckBox chkDeadCell3;
        private System.Windows.Forms.CheckBox chkDeadCell6;
        private System.Windows.Forms.CheckBox chkDeadCell5;
        private System.Windows.Forms.CheckBox chkDeadCell8;
        private System.Windows.Forms.CheckBox chkDeadCell7;
        private System.Windows.Forms.CheckBox chkLiveCell1;
        private System.Windows.Forms.CheckBox chkLiveCell2;
        private System.Windows.Forms.CheckBox chkLiveCell4;
        private System.Windows.Forms.CheckBox chkLiveCell3;
        private System.Windows.Forms.CheckBox chkLiveCell6;
        private System.Windows.Forms.CheckBox chkLiveCell5;
        private System.Windows.Forms.CheckBox chkLiveCell8;
        private System.Windows.Forms.CheckBox chkLiveCell7;
        private System.Windows.Forms.GroupBox grpLiveCells;
        private System.Windows.Forms.GroupBox grpDeadCells;
        private System.Windows.Forms.Button btnRestoreRuleDefaults;
        private System.Windows.Forms.Label lblGridSizeWidth;
        private System.Windows.Forms.Label lblGridSizeHeight;
        private System.Windows.Forms.NumericUpDown edtGridWidth;
        private System.Windows.Forms.NumericUpDown edtGridHeight;
        private System.Windows.Forms.GroupBox grpCellSize;
        private System.Windows.Forms.Label lblColourBackground;
        private System.Windows.Forms.Label lblColourLivingCells;
        private System.Windows.Forms.GroupBox grpColour;
        private System.Windows.Forms.Label lblAnimationDelay;
        private System.Windows.Forms.NumericUpDown edtAnimationDelay;
        private System.Windows.Forms.GroupBox grpAnimation;
        private System.Windows.Forms.Button btnRestoreGeneralDefaults;
        private System.Windows.Forms.Button btnClose;
        private System.Windows.Forms.Button btnApply;
        private System.Windows.Forms.TabPage tabRules;
        private System.Windows.Forms.TabPage tabGeneral;
        private System.Windows.Forms.TabControl tabsMain;
    }
}
