#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TTTreeCompareDlg
    {
        private TextView ListCompare;
        private Label lblFile;
        private TextField txtCompareFile;
        private Button btnFileChoose;
        private RadioButton radAnalysis;
        private Button btnMatch;
        private RadioButton radMathExternal;
        private RadioButton radMatchInternal;
        private FrameView grpMatchType;

        private void InitializeComponent()
        {
            btnMatch = new Button();
            grpMatchType = new FrameView();
            radAnalysis = new RadioButton();
            lblFile = new Label();
            txtCompareFile = new TextField();
            btnFileChoose = new Button();
            radMatchInternal = new RadioButton();
            radMathExternal = new RadioButton();
            ListCompare = new TextView();

            Add(btnMatch);
            Add(grpMatchType);
            Add(ListCompare);

            btnMatch.Location = new Point(84, 8);
            btnMatch.Size = new Size(10, 1);
            btnMatch.TabIndex = 7;
            btnMatch.Clicked += btnMatch_Click;

            grpMatchType.Add(radAnalysis);
            grpMatchType.Add(lblFile);
            grpMatchType.Add(txtCompareFile);
            grpMatchType.Add(btnFileChoose);
            grpMatchType.Add(radMatchInternal);
            grpMatchType.Add(radMathExternal);
            grpMatchType.Location = new Point(1, 0);
            grpMatchType.Size = new Size(80, 10);
            grpMatchType.TabIndex = 6;
            grpMatchType.TabStop = false;

            radMatchInternal.Checked = true;
            radMatchInternal.Location = new Point(1, 1);
            radMatchInternal.TabIndex = 2;
            radMatchInternal.TabStop = true;
            radMatchInternal.CheckedChanged += rbtnMatch_CheckedChanged;
            radMatchInternal.Group = "match-type";

            radMathExternal.Location = new Point(1, 3);
            radMathExternal.TabIndex = 3;
            radMathExternal.CheckedChanged += rbtnMatch_CheckedChanged;
            radMathExternal.Group = "match-type";

            lblFile.Location = new Point(3, 4);
            lblFile.TabIndex = 4;

            txtCompareFile.Enabled = false;
            txtCompareFile.Location = new Point(12, 4);
            txtCompareFile.ReadOnly = true;
            txtCompareFile.Size = new Size(40, 1);
            txtCompareFile.TabIndex = 5;

            btnFileChoose.Enabled = false;
            btnFileChoose.Location = new Point(54, 4);
            btnFileChoose.Size = new Size(10, 1);
            btnFileChoose.TabIndex = 6;
            btnFileChoose.Clicked += btnFileChoose_Click;

            radAnalysis.Location = new Point(1, 6);
            radAnalysis.TabIndex = 7;
            radAnalysis.CheckedChanged += rbtnMatch_CheckedChanged;
            radAnalysis.Group = "match-type";

            ListCompare.Location = new Point(1, 11);
            ListCompare.Multiline = true;
            ListCompare.ReadOnly = true;
            ListCompare.Size = new Size(96, 40);
            ListCompare.TabIndex = 0;

            Size = new Size(100, 54);
        }
    }
}
