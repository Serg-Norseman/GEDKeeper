#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class RepositoryCitEditDlg
    {
        private FrameView GroupBox1;
        private ComboBox cmbRepository;
        private Label lblRepository;
        private TabView tabsData;
        private TabView.Tab pageCallNumbers;
        private Button btnAccept;
        private Button btnCancel;
        private Button btnRepositoryAdd;

        private void InitializeComponent()
        {
            GroupBox1 = new FrameView();
            lblRepository = new Label();
            cmbRepository = new ComboBox();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabView();
            pageCallNumbers = new TabView.Tab();
            btnRepositoryAdd = new Button();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 2;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 3;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            GroupBox1.Add(lblRepository);
            GroupBox1.Add(cmbRepository);
            GroupBox1.Add(btnRepositoryAdd);
            GroupBox1.Location = new Point(0, 0);
            GroupBox1.Size = new Size(67, 5);
            GroupBox1.TabIndex = 0;
            GroupBox1.TabStop = false;

            lblRepository.Location = new Point(1, 1);
            lblRepository.TabIndex = 0;

            cmbRepository.Location = new Point(16, 1);
            cmbRepository.Size = new Size(42, 2);
            cmbRepository.TabIndex = 0;
            cmbRepository.KeyUp += cmbRepository_KeyUp;

            btnRepositoryAdd.Text = "+";
            btnRepositoryAdd.Location = new Point(59, 1);
            btnRepositoryAdd.Size = new Size(5, 1);
            btnRepositoryAdd.TabIndex = 2;
            btnRepositoryAdd.TabStop = false;
            btnRepositoryAdd.Clicked += btnRepositoryAdd_Click;

            tabsData.AddTab(pageCallNumbers);
            tabsData.Location = new Point(0, 5);
            tabsData.Size = new Size(67, 20);
            tabsData.TabIndex = 1;

            Size = new Size(69, 29);
            Add(tabsData);
            Add(GroupBox1);
        }
    }
}
