using System;
using System.ComponentModel;
using System.Windows.Forms;

namespace GKCommon.Validation
{
    public abstract class ValidatorBase : Component
    {
        private Control _controlToValidate;
        private string _errorMessage = "";
        private ErrorProvider _errorProvider = new ErrorProvider();
        private bool _isValid;

        [Description("Gets or sets the text for the error message."), DefaultValue(""), Category("Appearance")]
        public string ErrorMessage
        {
            get
            {
                return this._errorMessage;
            }
            set
            {
                this._errorMessage = value;
            }
        }

        [Category("Behaviour"), DefaultValue(null), TypeConverter(typeof(ValidatableControlConverter)), Description("Gets or sets the input control to validate.")]
        public Control ControlToValidate
        {
            get
            {
                return this._controlToValidate;
            }
            set
            {
                this._controlToValidate = value;
                if (this._controlToValidate != null && !base.DesignMode)
                {
                    this._controlToValidate.Validating += new CancelEventHandler(this.ControlToValidate_Validating);
                    this._controlToValidate.Validated += new EventHandler(this.ControlToValidate_Validated);
                }
            }
        }

        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public bool IsValid
        {
            get
            {
                return this._isValid;
            }
            set
            {
                this._isValid = value;
            }
        }

        public ValidatorBase()
        {
            this._errorProvider.BlinkStyle = ErrorBlinkStyle.NeverBlink;
        }

        protected abstract bool EvaluateIsValid();

        private void ControlToValidate_Validating(object sender, CancelEventArgs e)
        {
            this._isValid = this.EvaluateIsValid();
            if (!this._isValid)
            {
                this._errorProvider.SetError(this._controlToValidate, this.ErrorMessage);
                e.Cancel = true;
            }
        }

        private void ControlToValidate_Validated(object sender, EventArgs e)
        {
            this._errorProvider.SetError(this._controlToValidate, "");
        }
    }
}
