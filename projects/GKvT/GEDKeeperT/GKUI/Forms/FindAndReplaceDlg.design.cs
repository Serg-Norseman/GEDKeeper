#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class FindAndReplaceDlg
    {
        private Button btnReplace;
        private Button btnReplaceAll;
        private FrameView gbFilters;
        private Label lblPattern;
        private ComboBox cmbPattern;
        private CheckBox chkMatchCase;
        private CheckBox chkMatchWildcards;
        private Label lblReplacement;
        private ComboBox cmbReplacement;
        private CheckBox chkWholeWord;
        private Label lblProperty;
        private ComboBox cmbProperty;
        private Label lblRecord;
        private ComboBox cmbRecord;
        private Button btnPrev;
        private Button btnNext;

        private void InitializeComponent()
        {
            btnReplace = new Button();
            btnReplaceAll = new Button();
            gbFilters = new FrameView();
            lblProperty = new Label();
            cmbProperty = new ComboBox();
            lblRecord = new Label();
            cmbRecord = new ComboBox();
            lblPattern = new Label();
            cmbPattern = new ComboBox();
            chkMatchCase = new CheckBox();
            chkMatchWildcards = new CheckBox();
            lblReplacement = new Label();
            cmbReplacement = new ComboBox();
            chkWholeWord = new CheckBox();
            btnPrev = new Button();
            btnNext = new Button();

            lblRecord.Location = new Point(1, 1);
            lblRecord.TabIndex = 8;

            cmbRecord.Location = new Point(14, 1);
            cmbRecord.Size = new Size(23, 2);
            cmbRecord.TabIndex = 9;

            lblProperty.Location = new Point(1, 3);
            lblProperty.TabIndex = 10;

            cmbProperty.Location = new Point(14, 3);
            cmbProperty.Size = new Size(23, 2);
            cmbProperty.TabIndex = 11;

            gbFilters.Add(lblProperty);
            gbFilters.Add(cmbProperty);
            gbFilters.Add(lblRecord);
            gbFilters.Add(cmbRecord);
            gbFilters.Location = new Point(33, 5);
            gbFilters.Size = new Size(44, 7);
            gbFilters.TabIndex = 7;
            gbFilters.TabStop = false;

            lblPattern.Location = new Point(1, 1);
            lblPattern.TabIndex = 0;

            cmbPattern.Location = new Point(20, 1);
            cmbPattern.Size = new Size(57, 2);
            cmbPattern.TabIndex = 1;

            lblReplacement.Location = new Point(1, 3);
            lblReplacement.TabIndex = 2;

            cmbReplacement.Location = new Point(20, 3);
            cmbReplacement.Size = new Size(57, 2);
            cmbReplacement.TabIndex = 3;

            chkMatchCase.Location = new Point(1, 6);
            chkMatchCase.TabIndex = 4;

            chkMatchWildcards.Location = new Point(1, 8);
            chkMatchWildcards.TabIndex = 5;

            chkWholeWord.Location = new Point(1, 10);
            chkWholeWord.TabIndex = 6;

            btnPrev.Location = new Point(1, 13);
            btnPrev.Size = new Size(14, 1);
            btnPrev.TabIndex = 12;
            btnPrev.Clicked += btnPrev_Click;

            btnNext.Location = new Point(14, 13);
            btnNext.Size = new Size(14, 1);
            btnNext.TabIndex = 13;
            btnNext.Clicked += btnNext_Click;

            btnReplace.Location = new Point(40, 13);
            btnReplace.Size = new Size(14, 1);
            btnReplace.TabIndex = 14;
            btnReplace.Clicked += btnReplace_Click;

            btnReplaceAll.Location = new Point(60, 13);
            btnReplaceAll.Size = new Size(14, 1);
            btnReplaceAll.TabIndex = 15;
            btnReplaceAll.Clicked += btnReplaceAll_Click;

            Size = new Size(80, 16);
            Add(btnPrev);
            Add(btnNext);
            Add(chkWholeWord);
            Add(lblReplacement);
            Add(cmbReplacement);
            Add(lblPattern);
            Add(cmbPattern);
            Add(chkMatchCase);
            Add(chkMatchWildcards);
            Add(btnReplace);
            Add(btnReplaceAll);
            Add(gbFilters);
            KeyDown += Form_KeyDown;
        }
    }
}
