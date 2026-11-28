/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Input;
using Microsoft.Maui;
using Microsoft.Maui.ApplicationModel;
using Microsoft.Maui.Controls;
using Microsoft.Maui.Graphics;

namespace GKUI.Components;

public class HyperLabel : Label
{
    public static readonly BindableProperty UrlProperty =
        BindableProperty.Create(nameof(Url), typeof(string), typeof(HyperLabel), null);

    public static readonly BindableProperty CommandProperty =
        BindableProperty.Create(nameof(Command), typeof(ICommand), typeof(HyperLabel), null);

    public static readonly BindableProperty CommandParameterProperty =
        BindableProperty.Create(nameof(CommandParameter), typeof(object), typeof(HyperLabel), null);

    public string? Url
    {
        get => (string?)GetValue(UrlProperty);
        set => SetValue(UrlProperty, value);
    }

    public ICommand? Command
    {
        get => (ICommand?)GetValue(CommandProperty);
        set => SetValue(CommandProperty, value);
    }

    public object? CommandParameter
    {
        get => GetValue(CommandParameterProperty);
        set => SetValue(CommandParameterProperty, value);
    }

    public HyperLabel()
    {
        TextDecorations = TextDecorations.Underline;
        TextColor = Colors.Blue;

        var tapGesture = new TapGestureRecognizer();
        tapGesture.Tapped += OnTapped;
        GestureRecognizers.Add(tapGesture);
    }

    private async void OnTapped(object? sender, TappedEventArgs e)
    {
        if (Command?.CanExecute(CommandParameter) == true) {
            Command.Execute(CommandParameter);
        }

        if (!string.IsNullOrWhiteSpace(Url) && Uri.TryCreate(Url, UriKind.Absolute, out var uri)) {
            await Launcher.OpenAsync(uri);
        }
    }
}
