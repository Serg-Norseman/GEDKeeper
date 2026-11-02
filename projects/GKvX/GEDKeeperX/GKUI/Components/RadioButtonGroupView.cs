using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Maui;
using Microsoft.Maui.Controls;

namespace GKUI.Components
{
    public class RadioButtonGroupView : StackLayout
    {
        public static readonly BindableProperty SelectedRadioButtonProperty =
            BindableProperty.Create(
                nameof(SelectedRadioButton),
                typeof(RadioButton),
                typeof(RadioButtonGroupView),
                default(RadioButton),
                BindingMode.TwoWay,
                propertyChanged: OnSelectedRadioButtonChanged);

        public RadioButton SelectedRadioButton
        {
            get => (RadioButton)GetValue(SelectedRadioButtonProperty);
            set => SetValue(SelectedRadioButtonProperty, value);
        }

        public event EventHandler<CheckedChangedEventArgs> CheckedChanged;

        public RadioButtonGroupView()
        {
            ChildAdded += OnChildAdded;
            ChildRemoved += OnChildRemoved;
        }

        private void OnChildAdded(object sender, ElementEventArgs e)
        {
            RegisterRadioButtons(e.Element);
        }

        private void OnChildRemoved(object sender, ElementEventArgs e)
        {
            UnregisterRadioButtons(e.Element);
        }

        private void RegisterRadioButtons(Element element)
        {
            if (element is RadioButton radioButton) {
                radioButton.Checked += OnRadioButtonCheckedChanged;

                if (radioButton.IsChecked) {
                    SelectedRadioButton = radioButton;
                }
            } else if (element is IVisualTreeElement visualElement) {
                foreach (var child in visualElement.GetVisualChildren().OfType<Element>()) {
                    RegisterRadioButtons(child);
                }
            }
        }

        private void UnregisterRadioButtons(Element element)
        {
            if (element is RadioButton radioButton) {
                radioButton.Checked -= OnRadioButtonCheckedChanged;
                if (SelectedRadioButton == radioButton) {
                    SelectedRadioButton = null;
                }
            } else if (element is IVisualTreeElement visualElement) {
                foreach (var child in visualElement.GetVisualChildren().OfType<Element>()) {
                    UnregisterRadioButtons(child);
                }
            }
        }

        private bool _isUpdatingGroup = false;

        private void OnRadioButtonCheckedChanged(object sender, CheckedChangedEventArgs e)
        {
            if (_isUpdatingGroup) return;

            if (sender is RadioButton clickedRadio && e.Value) {
                _isUpdatingGroup = true;

                try {
                    foreach (var radio in FindAllRadioButtons(this)) {
                        if (radio != clickedRadio) {
                            radio.IsChecked = false;
                        }
                    }

                    SelectedRadioButton = clickedRadio;
                    CheckedChanged?.Invoke(this, e);
                } finally {
                    _isUpdatingGroup = false;
                }
            }
        }

        private static void OnSelectedRadioButtonChanged(BindableObject bindable, object oldValue, object newValue)
        {
            var groupView = (RadioButtonGroupView)bindable;
            if (groupView._isUpdatingGroup) return;

            if (newValue is RadioButton targetRadio) {
                groupView._isUpdatingGroup = true;
                try {
                    foreach (var radio in groupView.FindAllRadioButtons(groupView)) {
                        radio.IsChecked = radio == targetRadio;
                    }
                } finally {
                    groupView._isUpdatingGroup = false;
                }
            }
        }

        private IEnumerable<RadioButton> FindAllRadioButtons(Element element)
        {
            if (element is RadioButton radio) {
                yield return radio;
            } else if (element is IVisualTreeElement visualElement) {
                foreach (var child in visualElement.GetVisualChildren().OfType<Element>()) {
                    foreach (var foundRadio in FindAllRadioButtons(child)) {
                        yield return foundRadio;
                    }
                }
            }
        }
    }
}
