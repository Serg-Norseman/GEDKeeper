using System;
using Microsoft.Maui.Controls;

namespace GKUI.Components
{
    public class RadioButton : ContentView
    {
        private readonly Microsoft.Maui.Controls.RadioButton _nativeRadioButton;
        private readonly Label _label;
        private readonly HorizontalStackLayout _layout;
        private LabelPosition _textPosition = LabelPosition.Before;

        public LabelPosition LabelPosition
        {
            get => _textPosition;
            set { _textPosition = value; UpdateLayout(); }
        }

        public static readonly BindableProperty IsCheckedProperty =
            BindableProperty.Create(nameof(IsChecked), typeof(bool), typeof(RadioButton), false,
                propertyChanged: (b, o, n) => ((RadioButton)b)._nativeRadioButton.IsChecked = (bool)n);

        public bool IsChecked
        {
            get => (bool)GetValue(IsCheckedProperty);
            set => SetValue(IsCheckedProperty, value);
        }

        public static readonly BindableProperty TextProperty =
            BindableProperty.Create(nameof(Text), typeof(string), typeof(RadioButton), string.Empty,
                propertyChanged: (b, o, n) => ((RadioButton)b)._label.Text = (string)n);

        public string Text
        {
            get => (string)GetValue(TextProperty);
            set => SetValue(TextProperty, value);
        }

        public event EventHandler<CheckedChangedEventArgs> Checked;

        public bool IsDisabled { get; set; }

        public RadioButton()
        {
            _nativeRadioButton = new Microsoft.Maui.Controls.RadioButton { VerticalOptions = LayoutOptions.Center };
            _label = new Label { VerticalOptions = LayoutOptions.Center };

            _nativeRadioButton.CheckedChanged += (s, e) => {
                IsChecked = e.Value;
                Checked?.Invoke(this, new CheckedChangedEventArgs(e.Value));
            };

            _layout = new HorizontalStackLayout { Spacing = 10, VerticalOptions = LayoutOptions.Center };
            Content = _layout;

            UpdateLayout();
        }

        private void UpdateLayout()
        {
            _layout.Children.Clear();
            if (_textPosition == LabelPosition.Before) {
                _layout.Children.Add(_label);
                _layout.Children.Add(_nativeRadioButton);
            } else {
                _layout.Children.Add(_nativeRadioButton);
                _layout.Children.Add(_label);
            }
        }
    }
}
