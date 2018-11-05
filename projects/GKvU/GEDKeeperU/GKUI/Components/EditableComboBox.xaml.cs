using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Data;

namespace GKUI.Components
{
    /// <summary>
    /// UserControl where a ComboBox also has an editable TextBox.
    /// https://liftcodeplay.com/2016/02/18/combobox-with-editable-textbox/
    /// </summary>
    public sealed partial class EditableComboBox : UserControl
    {
        public EditableComboBox()
        {
            //<Button x:Name="DeleteButton" BorderThickness="{TemplateBinding BorderThickness}"                                     Grid.Column="1" FontSize="{TemplateBinding FontSize}" IsTabStop="False"                                     Margin="{ThemeResource HelperButtonThemePadding}"                                     MinWidth="34" Grid.Row="1"                                     Style="{StaticResource DeleteButtonStyle}" Visibility="Collapsed"                                     VerticalAlignment="Stretch" />
            InitializeComponent();
        }

        public object ComboBoxTextItems
        {
            set { comboBox.SetBinding(ItemsControl.ItemsSourceProperty, value as Binding); }
        }

        public object SelectedComboBoxTextItem
        {
            set { textBox.SetBinding(TextBox.TextProperty, value as Binding); }
        }

        public bool IsEditable
        {
            get { return true; }
            set {  }
        }

        public ItemCollection Items
        {
            get { return comboBox.Items; }
        }

        public int SelectedIndex
        {
            get { return comboBox.SelectedIndex; }
            set { comboBox.SelectedIndex = value; }
        }

        public object SelectedItem
        {
            get { return comboBox.SelectedItem; }
            set { comboBox.SelectedItem = value; }
        }

        public string Text
        {
            get { return textBox.Text; }
            set { textBox.Text = value; }
        }

        private void comboBox_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            textBox.Text = comboBox.SelectedItem as string;
        }
    }
}
