﻿namespace GKUI.Forms
{
    partial class TreeChartWin
    {
        private System.Windows.Forms.ToolStrip ToolBar1;
        private System.Windows.Forms.ToolStripButton tbImageSave;
        private System.Windows.Forms.ContextMenuStrip MenuPerson;
        private System.Windows.Forms.ToolStripMenuItem miEdit;
        private System.Windows.Forms.ToolStripSeparator N1;
        private System.Windows.Forms.ToolStripMenuItem miSpouseAdd;
        private System.Windows.Forms.ToolStripMenuItem miSonAdd;
        private System.Windows.Forms.ToolStripMenuItem miDaughterAdd;
        private System.Windows.Forms.ToolStripMenuItem miFamilyAdd;
        private System.Windows.Forms.ToolStripSeparator N2;
        private System.Windows.Forms.ToolStripMenuItem miDelete;
        private System.Windows.Forms.ToolStripSeparator N3;
        private System.Windows.Forms.ToolStripMenuItem miRebuildKinships;
        private System.Windows.Forms.ToolStripDropDownButton tbModes;
        private System.Windows.Forms.ContextMenuStrip MenuModes;
        private System.Windows.Forms.ToolStripMenuItem miModeBoth;
        private System.Windows.Forms.ToolStripMenuItem miModeAncestors;
        private System.Windows.Forms.ToolStripMenuItem miModeDescendants;
        private System.Windows.Forms.ToolStripSeparator N7;
        private System.Windows.Forms.ToolStripMenuItem miTraceSelected;
        private System.Windows.Forms.ToolStripMenuItem miTraceKinships;
        private System.Windows.Forms.ToolStripMenuItem miCertaintyIndex;
        private System.Windows.Forms.ToolStripMenuItem miRebuildTree;
        private System.Windows.Forms.ToolStripDropDownButton tbGens;
        private System.Windows.Forms.ContextMenuStrip MenuGens;
        private System.Windows.Forms.ToolStripMenuItem miGensInf;
        private System.Windows.Forms.ToolStripMenuItem miGens1;
        private System.Windows.Forms.ToolStripMenuItem miGens2;
        private System.Windows.Forms.ToolStripMenuItem miGens3;
        private System.Windows.Forms.ToolStripMenuItem miGens4;
        private System.Windows.Forms.ToolStripMenuItem miGens5;
        private System.Windows.Forms.ToolStripMenuItem miGens6;
        private System.Windows.Forms.ToolStripMenuItem miGens7;
        private System.Windows.Forms.ToolStripMenuItem miGens8;
        private System.Windows.Forms.ToolStripMenuItem miGens9;
        private System.Windows.Forms.ToolStripSeparator N8;
        private System.Windows.Forms.ToolStripMenuItem miFillColor;
        private System.Windows.Forms.ToolStripMenuItem miFillImage;
        private System.Windows.Forms.ToolStripSeparator N9;
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.ToolStripSeparator tbs2;
        private System.Windows.Forms.ToolStripSeparator tbs1;
        private System.Windows.Forms.ToolStripMenuItem miFatherAdd;
        private System.Windows.Forms.ToolStripMenuItem miMotherAdd;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripButton tbDocPrint;
        private System.Windows.Forms.ToolStripButton tbDocPreview;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripButton tbFilter;
        private System.Windows.Forms.ToolStripButton tbPrev;
        private System.Windows.Forms.ToolStripButton tbNext;
        private System.Windows.Forms.ToolStripSeparator N10;
        private System.Windows.Forms.ToolStripMenuItem miSelectColor;

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.ToolBar1 = new System.Windows.Forms.ToolStrip();
            this.tbImageSave = new System.Windows.Forms.ToolStripButton();
            this.tbs1 = new System.Windows.Forms.ToolStripSeparator();
            this.tbGens = new System.Windows.Forms.ToolStripDropDownButton();
            this.MenuGens = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miGensInf = new System.Windows.Forms.ToolStripMenuItem();
            this.miGens1 = new System.Windows.Forms.ToolStripMenuItem();
            this.miGens2 = new System.Windows.Forms.ToolStripMenuItem();
            this.miGens3 = new System.Windows.Forms.ToolStripMenuItem();
            this.miGens4 = new System.Windows.Forms.ToolStripMenuItem();
            this.miGens5 = new System.Windows.Forms.ToolStripMenuItem();
            this.miGens6 = new System.Windows.Forms.ToolStripMenuItem();
            this.miGens7 = new System.Windows.Forms.ToolStripMenuItem();
            this.miGens8 = new System.Windows.Forms.ToolStripMenuItem();
            this.miGens9 = new System.Windows.Forms.ToolStripMenuItem();
            this.tbs2 = new System.Windows.Forms.ToolStripSeparator();
            this.tbModes = new System.Windows.Forms.ToolStripDropDownButton();
            this.MenuModes = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miModeBoth = new System.Windows.Forms.ToolStripMenuItem();
            this.miModeAncestors = new System.Windows.Forms.ToolStripMenuItem();
            this.miModeDescendants = new System.Windows.Forms.ToolStripMenuItem();
            this.N7 = new System.Windows.Forms.ToolStripSeparator();
            this.miTraceSelected = new System.Windows.Forms.ToolStripMenuItem();
            this.miTraceKinships = new System.Windows.Forms.ToolStripMenuItem();
            this.miCertaintyIndex = new System.Windows.Forms.ToolStripMenuItem();
            this.N8 = new System.Windows.Forms.ToolStripSeparator();
            this.miFillColor = new System.Windows.Forms.ToolStripMenuItem();
            this.miFillImage = new System.Windows.Forms.ToolStripMenuItem();
            this.N9 = new System.Windows.Forms.ToolStripSeparator();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.tbFilter = new System.Windows.Forms.ToolStripButton();
            this.tbPrev = new System.Windows.Forms.ToolStripButton();
            this.tbNext = new System.Windows.Forms.ToolStripButton();
            this.tbDocPreview = new System.Windows.Forms.ToolStripButton();
            this.tbDocPrint = new System.Windows.Forms.ToolStripButton();
            this.MenuPerson = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miEdit = new System.Windows.Forms.ToolStripMenuItem();
            this.N1 = new System.Windows.Forms.ToolStripSeparator();
            this.miFatherAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miMotherAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miFamilyAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miSpouseAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miSonAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miDaughterAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.N2 = new System.Windows.Forms.ToolStripSeparator();
            this.miDelete = new System.Windows.Forms.ToolStripMenuItem();
            this.N3 = new System.Windows.Forms.ToolStripSeparator();
            this.miRebuildTree = new System.Windows.Forms.ToolStripMenuItem();
            this.miRebuildKinships = new System.Windows.Forms.ToolStripMenuItem();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.N10 = new System.Windows.Forms.ToolStripSeparator();
            this.miSelectColor = new System.Windows.Forms.ToolStripMenuItem();
            this.ToolBar1.SuspendLayout();
            this.MenuGens.SuspendLayout();
            this.MenuModes.SuspendLayout();
            this.MenuPerson.SuspendLayout();
            this.SuspendLayout();
            // 
            // ToolBar1
            // 
            this.ToolBar1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.ToolBar1.ImageScalingSize = new System.Drawing.Size(20, 20);
            this.ToolBar1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                             this.tbImageSave,
                                             this.tbs1,
                                             this.tbGens,
                                             this.tbs2,
                                             this.tbModes,
                                             this.toolStripSeparator1,
                                             this.tbFilter,
                                             this.tbPrev,
                                             this.tbNext,
                                             this.toolStripSeparator2,
                                             this.tbDocPreview,
                                             this.tbDocPrint});
            this.ToolBar1.Location = new System.Drawing.Point(0, 0);
            this.ToolBar1.Name = "ToolBar1";
            this.ToolBar1.Size = new System.Drawing.Size(822, 27);
            this.ToolBar1.TabIndex = 0;
            // 
            // tbImageSave
            // 
            this.tbImageSave.Name = "tbImageSave";
            this.tbImageSave.Size = new System.Drawing.Size(23, 24);
            this.tbImageSave.Click += new System.EventHandler(this.tbImageSave_Click);
            // 
            // tbs1
            // 
            this.tbs1.Name = "tbs1";
            this.tbs1.Size = new System.Drawing.Size(6, 27);
            // 
            // tbGens
            // 
            this.tbGens.DropDown = this.MenuGens;
            this.tbGens.Name = "tbGens";
            this.tbGens.Size = new System.Drawing.Size(68, 24);
            this.tbGens.Text = "tbGens";
            // 
            // MenuGens
            // 
            this.MenuGens.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                             this.miGensInf,
                                             this.miGens1,
                                             this.miGens2,
                                             this.miGens3,
                                             this.miGens4,
                                             this.miGens5,
                                             this.miGens6,
                                             this.miGens7,
                                             this.miGens8,
                                             this.miGens9});
            this.MenuGens.Name = "MenuGens";
            this.MenuGens.OwnerItem = this.tbGens;
            this.MenuGens.Size = new System.Drawing.Size(96, 244);
            // 
            // miGensInf
            // 
            this.miGensInf.Checked = true;
            this.miGensInf.CheckState = System.Windows.Forms.CheckState.Checked;
            this.miGensInf.Name = "miGensInf";
            this.miGensInf.Size = new System.Drawing.Size(95, 24);
            this.miGensInf.Text = "Inf";
            this.miGensInf.Click += new System.EventHandler(this.miGens9_Click);
            // 
            // miGens1
            // 
            this.miGens1.Name = "miGens1";
            this.miGens1.Size = new System.Drawing.Size(95, 24);
            this.miGens1.Text = "1";
            this.miGens1.Click += new System.EventHandler(this.miGens9_Click);
            // 
            // miGens2
            // 
            this.miGens2.Name = "miGens2";
            this.miGens2.Size = new System.Drawing.Size(95, 24);
            this.miGens2.Text = "2";
            this.miGens2.Click += new System.EventHandler(this.miGens9_Click);
            // 
            // miGens3
            // 
            this.miGens3.Name = "miGens3";
            this.miGens3.Size = new System.Drawing.Size(95, 24);
            this.miGens3.Text = "3";
            this.miGens3.Click += new System.EventHandler(this.miGens9_Click);
            // 
            // miGens4
            // 
            this.miGens4.Name = "miGens4";
            this.miGens4.Size = new System.Drawing.Size(95, 24);
            this.miGens4.Text = "4";
            this.miGens4.Click += new System.EventHandler(this.miGens9_Click);
            // 
            // miGens5
            // 
            this.miGens5.Name = "miGens5";
            this.miGens5.Size = new System.Drawing.Size(95, 24);
            this.miGens5.Text = "5";
            this.miGens5.Click += new System.EventHandler(this.miGens9_Click);
            // 
            // miGens6
            // 
            this.miGens6.Name = "miGens6";
            this.miGens6.Size = new System.Drawing.Size(95, 24);
            this.miGens6.Text = "6";
            this.miGens6.Click += new System.EventHandler(this.miGens9_Click);
            // 
            // miGens7
            // 
            this.miGens7.Name = "miGens7";
            this.miGens7.Size = new System.Drawing.Size(95, 24);
            this.miGens7.Text = "7";
            this.miGens7.Click += new System.EventHandler(this.miGens9_Click);
            // 
            // miGens8
            // 
            this.miGens8.Name = "miGens8";
            this.miGens8.Size = new System.Drawing.Size(95, 24);
            this.miGens8.Text = "8";
            this.miGens8.Click += new System.EventHandler(this.miGens9_Click);
            // 
            // miGens9
            // 
            this.miGens9.Name = "miGens9";
            this.miGens9.Size = new System.Drawing.Size(95, 24);
            this.miGens9.Text = "9";
            this.miGens9.Click += new System.EventHandler(this.miGens9_Click);
            // 
            // tbs2
            // 
            this.tbs2.Name = "tbs2";
            this.tbs2.Size = new System.Drawing.Size(6, 27);
            // 
            // tbModes
            // 
            this.tbModes.DropDown = this.MenuModes;
            this.tbModes.Name = "tbModes";
            this.tbModes.Size = new System.Drawing.Size(13, 24);
            // 
            // MenuModes
            // 
            this.MenuModes.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                              this.miModeBoth,
                                              this.miModeAncestors,
                                              this.miModeDescendants,
                                              this.N7,
                                              this.miTraceSelected,
                                              this.miTraceKinships,
                                              this.miCertaintyIndex,
                                              this.N8,
                                              this.miFillColor,
                                              this.miFillImage,
                                              this.N9});
            this.MenuModes.Name = "MenuModes";
            this.MenuModes.OwnerItem = this.tbModes;
            this.MenuModes.Size = new System.Drawing.Size(219, 214);
            // 
            // miModeBoth
            // 
            this.miModeBoth.Name = "miModeBoth";
            this.miModeBoth.Size = new System.Drawing.Size(218, 24);
            this.miModeBoth.Text = "miModeBoth";
            this.miModeBoth.Click += new System.EventHandler(this.miModeItem_Click);
            // 
            // miModeAncestors
            // 
            this.miModeAncestors.Name = "miModeAncestors";
            this.miModeAncestors.Size = new System.Drawing.Size(218, 24);
            this.miModeAncestors.Text = "miModeAncestors";
            this.miModeAncestors.Click += new System.EventHandler(this.miModeItem_Click);
            // 
            // miModeDescendants
            // 
            this.miModeDescendants.Name = "miModeDescendants";
            this.miModeDescendants.Size = new System.Drawing.Size(218, 24);
            this.miModeDescendants.Text = "miModeDescendants";
            this.miModeDescendants.Click += new System.EventHandler(this.miModeItem_Click);
            // 
            // N7
            // 
            this.N7.Name = "N7";
            this.N7.Size = new System.Drawing.Size(215, 6);
            // 
            // miTraceSelected
            // 
            this.miTraceSelected.Name = "miTraceSelected";
            this.miTraceSelected.Size = new System.Drawing.Size(218, 24);
            this.miTraceSelected.Text = "miTraceSelected";
            this.miTraceSelected.Click += new System.EventHandler(this.miTraceSelected_Click);
            // 
            // miTraceKinships
            // 
            this.miTraceKinships.Name = "miTraceKinships";
            this.miTraceKinships.Size = new System.Drawing.Size(218, 24);
            this.miTraceKinships.Text = "miTraceKinships";
            this.miTraceKinships.Click += new System.EventHandler(this.miTraceKinships_Click);
            // 
            // miCertaintyIndex
            // 
            this.miCertaintyIndex.Name = "miCertaintyIndex";
            this.miCertaintyIndex.Size = new System.Drawing.Size(218, 24);
            this.miCertaintyIndex.Text = "miCertaintyIndex";
            this.miCertaintyIndex.Click += new System.EventHandler(this.miCertaintyIndex_Click);
            // 
            // N8
            // 
            this.N8.Name = "N8";
            this.N8.Size = new System.Drawing.Size(215, 6);
            // 
            // miFillColor
            // 
            this.miFillColor.Name = "miFillColor";
            this.miFillColor.Size = new System.Drawing.Size(218, 24);
            this.miFillColor.Text = "miFillColor";
            this.miFillColor.Click += new System.EventHandler(this.miFillColor_Click);
            // 
            // miFillImage
            // 
            this.miFillImage.Name = "miFillImage";
            this.miFillImage.Size = new System.Drawing.Size(218, 24);
            this.miFillImage.Text = "miFillImage";
            this.miFillImage.Click += new System.EventHandler(this.miFillImage_Click);
            // 
            // N9
            // 
            this.N9.Name = "N9";
            this.N9.Size = new System.Drawing.Size(215, 6);
            this.N9.Visible = false;
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 27);
            // 
            // tbFilter
            // 
            this.tbFilter.ImageTransparentColor = System.Drawing.Color.White;
            this.tbFilter.Name = "tbFilter";
            this.tbFilter.Size = new System.Drawing.Size(23, 24);
            this.tbFilter.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
            // 
            // tbPrev
            // 
            this.tbPrev.Enabled = false;
            this.tbPrev.Name = "tbPrev";
            this.tbPrev.Size = new System.Drawing.Size(23, 24);
            this.tbPrev.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
            // 
            // tbNext
            // 
            this.tbNext.Enabled = false;
            this.tbNext.Name = "tbNext";
            this.tbNext.Size = new System.Drawing.Size(23, 24);
            this.tbNext.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
            // 
            // tbDocPreview
            // 
            this.tbDocPreview.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbDocPreview.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbDocPreview.Name = "tbDocPreview";
            this.tbDocPreview.Size = new System.Drawing.Size(23, 24);
            this.tbDocPreview.Text = "toolStripButton1";
            this.tbDocPreview.Click += new System.EventHandler(this.tbDocPreview_Click);
            // 
            // tbDocPrint
            // 
            this.tbDocPrint.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbDocPrint.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbDocPrint.Name = "tbDocPrint";
            this.tbDocPrint.Size = new System.Drawing.Size(23, 24);
            this.tbDocPrint.Text = "toolStripButton2";
            this.tbDocPrint.Click += new System.EventHandler(this.tbDocPrint_Click);
            // 
            // MenuPerson
            // 
            this.MenuPerson.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                               this.miEdit,
                                               this.N1,
                                               this.miFatherAdd,
                                               this.miMotherAdd,
                                               this.miFamilyAdd,
                                               this.miSpouseAdd,
                                               this.miSonAdd,
                                               this.miDaughterAdd,
                                               this.N2,
                                               this.miDelete,
                                               this.N3,
                                               this.miRebuildTree,
                                               this.miRebuildKinships,
                                               this.N10,
                                               this.miSelectColor});
            this.MenuPerson.Name = "MenuPerson";
            this.MenuPerson.Size = new System.Drawing.Size(201, 262);
            this.MenuPerson.Opening += new System.ComponentModel.CancelEventHandler(this.MenuPerson_Opening);
            // 
            // miEdit
            // 
            this.miEdit.Name = "miEdit";
            this.miEdit.Size = new System.Drawing.Size(200, 24);
            this.miEdit.Text = "miEdit";
            this.miEdit.Click += new System.EventHandler(this.miEdit_Click);
            // 
            // N1
            // 
            this.N1.Name = "N1";
            this.N1.Size = new System.Drawing.Size(197, 6);
            // 
            // miFatherAdd
            // 
            this.miFatherAdd.Name = "miFatherAdd";
            this.miFatherAdd.Size = new System.Drawing.Size(200, 24);
            this.miFatherAdd.Text = "miFatherAdd";
            this.miFatherAdd.Click += new System.EventHandler(this.miFatherAdd_Click);
            // 
            // miMotherAdd
            // 
            this.miMotherAdd.Name = "miMotherAdd";
            this.miMotherAdd.Size = new System.Drawing.Size(200, 24);
            this.miMotherAdd.Text = "miMotherAdd";
            this.miMotherAdd.Click += new System.EventHandler(this.miMotherAdd_Click);
            // 
            // miFamilyAdd
            // 
            this.miFamilyAdd.Name = "miFamilyAdd";
            this.miFamilyAdd.Size = new System.Drawing.Size(200, 24);
            this.miFamilyAdd.Text = "miFamilyAdd";
            this.miFamilyAdd.Click += new System.EventHandler(this.miFamilyAdd_Click);
            // 
            // miSpouseAdd
            // 
            this.miSpouseAdd.Name = "miSpouseAdd";
            this.miSpouseAdd.Size = new System.Drawing.Size(200, 24);
            this.miSpouseAdd.Text = "miSpouseAdd";
            this.miSpouseAdd.Click += new System.EventHandler(this.miSpouseAdd_Click);
            // 
            // miSonAdd
            // 
            this.miSonAdd.Name = "miSonAdd";
            this.miSonAdd.Size = new System.Drawing.Size(200, 24);
            this.miSonAdd.Text = "miSonAdd";
            this.miSonAdd.Click += new System.EventHandler(this.miSonAdd_Click);
            // 
            // miDaughterAdd
            // 
            this.miDaughterAdd.Name = "miDaughterAdd";
            this.miDaughterAdd.Size = new System.Drawing.Size(200, 24);
            this.miDaughterAdd.Text = "miDaughterAdd";
            this.miDaughterAdd.Click += new System.EventHandler(this.miDaughterAdd_Click);
            // 
            // N2
            // 
            this.N2.Name = "N2";
            this.N2.Size = new System.Drawing.Size(197, 6);
            // 
            // miDelete
            // 
            this.miDelete.Name = "miDelete";
            this.miDelete.Size = new System.Drawing.Size(200, 24);
            this.miDelete.Text = "miDelete";
            this.miDelete.Click += new System.EventHandler(this.miDelete_Click);
            // 
            // N3
            // 
            this.N3.Name = "N3";
            this.N3.Size = new System.Drawing.Size(197, 6);
            // 
            // miRebuildTree
            // 
            this.miRebuildTree.Name = "miRebuildTree";
            this.miRebuildTree.Size = new System.Drawing.Size(200, 24);
            this.miRebuildTree.Text = "miRebuildTree";
            this.miRebuildTree.Click += new System.EventHandler(this.miRebuildTree_Click);
            // 
            // miRebuildKinships
            // 
            this.miRebuildKinships.Name = "miRebuildKinships";
            this.miRebuildKinships.Size = new System.Drawing.Size(200, 24);
            this.miRebuildKinships.Text = "miRebuildKinships";
            this.miRebuildKinships.Click += new System.EventHandler(this.miRebuildKinships_Click);
            // 
            // N10
            // 
            this.N10.Name = "N10";
            this.N10.Size = new System.Drawing.Size(197, 6);
            // 
            // miSelectColor
            // 
            this.miSelectColor.Name = "miSelectColor";
            this.miSelectColor.Size = new System.Drawing.Size(200, 24);
            this.miSelectColor.Text = "miSelectColor";
            this.miSelectColor.Click += new System.EventHandler(this.miSelectColor_Click);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(6, 27);
            // 
            // TreeChartWin
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(822, 452);
            this.Controls.Add(this.ToolBar1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.KeyPreview = true;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "TreeChartWin";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "TreeChartWin";
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TreeChartWin_KeyDown);
            this.ToolBar1.ResumeLayout(false);
            this.ToolBar1.PerformLayout();
            this.MenuGens.ResumeLayout(false);
            this.MenuModes.ResumeLayout(false);
            this.MenuPerson.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();
        }
    }
}
