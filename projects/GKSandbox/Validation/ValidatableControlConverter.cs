using System;
using System.ComponentModel;
using System.Windows.Forms;

namespace GKCommon.Validation
{
    public class ValidatableControlConverter : ReferenceConverter
    {
        public ValidatableControlConverter(Type type) : base(type)
        {
        }

        protected override bool IsValueAllowed(ITypeDescriptorContext context, object value)
        {
            return value is TextBox || value is ListBox || value is ComboBox || value is UserControl;
        }
    }
}
