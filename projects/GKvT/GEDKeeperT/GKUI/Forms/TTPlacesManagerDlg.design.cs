#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TTPlacesManagerDlg
    {
        private FrameView Panel4;
        private Button btnAnalysePlaces;
        private Button btnIntoList;
        private Label lblFilter;
        private TextField txtFilter;

        private void InitializeComponent()
        {
            Panel4 = new FrameView();
            btnIntoList = new Button();
            btnAnalysePlaces = new Button();
            lblFilter = new Label();
            txtFilter = new TextField();

            Panel4.Location = new Point(0, 0);
            Panel4.Size = new Size(100, 44);
            Panel4.TabIndex = 0;

            btnAnalysePlaces.Location = new Point(1, 47);
            btnAnalysePlaces.Size = new Size(14, 1);
            btnAnalysePlaces.TabIndex = 1;
            btnAnalysePlaces.Clicked += btnAnalysePlaces_Click;

            btnIntoList.Location = new Point(54, 47);
            btnIntoList.Size = new Size(14, 1);
            btnIntoList.TabIndex = 5;
            btnIntoList.Clicked += btnIntoList_Click;

            lblFilter.Location = new Point(18, 47);
            lblFilter.TabIndex = 2;

            txtFilter.Location = new Point(26, 47);
            txtFilter.Size = new Size(16, 1);
            txtFilter.TabIndex = 3;

            Add(Panel4);
            Add(btnIntoList);
            Add(btnAnalysePlaces);
            Add(lblFilter);
            Add(txtFilter);

            Size = new Size(102, 50);
            Loaded += Form_Load;
        }
    }
}
