using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using Microsoft.Maui;
using Microsoft.Maui.Controls;
using Microsoft.Maui.Graphics;

namespace GKUI.Components;


public class TabItem : ContentView
{
    public static readonly BindableProperty HeaderProperty =
        BindableProperty.Create(nameof(HeaderText), typeof(string), typeof(TabItem), string.Empty);

    public string HeaderText
    {
        get => (string)GetValue(HeaderProperty);
        set => SetValue(HeaderProperty, value);
    }
}


public partial class TabViewControl : ContentView
{
    private readonly HorizontalStackLayout TabsHeaderContainer;
    private readonly ContentPresenter TabContentPresenter;

    public ObservableCollection<TabItem> ItemSource { get; } = new ObservableCollection<TabItem>();

    public event EventHandler PositionChanged;

    #region Bindable Properties

    public static readonly BindableProperty SelectedTabIndexProperty =
        BindableProperty.Create(nameof(SelectedTabIndex), typeof(int), typeof(TabViewControl), 0,
            defaultBindingMode: BindingMode.TwoWay, propertyChanged: OnSelectedTabIndexChanged);

    public int SelectedTabIndex
    {
        get => (int)GetValue(SelectedTabIndexProperty);
        set => SetValue(SelectedTabIndexProperty, value);
    }

    public static readonly BindableProperty HeaderBackgroundColorProperty =
        BindableProperty.Create(nameof(HeaderBackgroundColor), typeof(Color), typeof(TabViewControl), Colors.Transparent,
            propertyChanged: (b, o, n) => ((TabViewControl)b).TabsHeaderContainer.Background = (Color)n);

    public Color HeaderBackgroundColor
    {
        get => (Color)GetValue(HeaderBackgroundColorProperty);
        set => SetValue(HeaderBackgroundColorProperty, value);
    }

    public static readonly BindableProperty SelectedTabColorProperty =
        BindableProperty.Create(nameof(SelectedTabColor), typeof(Color), typeof(TabViewControl), Colors.DeepSkyBlue,
            propertyChanged: (b, o, n) => ((TabViewControl)b).UpdateTabVisuals());

    public Color SelectedTabColor
    {
        get => (Color)GetValue(SelectedTabColorProperty);
        set => SetValue(SelectedTabColorProperty, value);
    }

    public static readonly BindableProperty UnselectedTabColorProperty =
        BindableProperty.Create(nameof(UnselectedTabColor), typeof(Color), typeof(TabViewControl), Colors.LightGray,
            propertyChanged: (b, o, n) => ((TabViewControl)b).UpdateTabVisuals());

    public Color UnselectedTabColor
    {
        get => (Color)GetValue(UnselectedTabColorProperty);
        set => SetValue(UnselectedTabColorProperty, value);
    }

    public static readonly BindableProperty TabTextColorProperty =
        BindableProperty.Create(nameof(TabTextColor), typeof(Color), typeof(TabViewControl), Colors.White,
            propertyChanged: (b, o, n) => ((TabViewControl)b).UpdateTabVisuals());

    public Color TabTextColor
    {
        get => (Color)GetValue(TabTextColorProperty);
        set => SetValue(TabTextColorProperty, value);
    }

    #endregion

    public TabViewControl()
    {
        TabsHeaderContainer = new HorizontalStackLayout {
            Spacing = 5,
            Background = Colors.Transparent,
            Padding = new Thickness(5)
        };
        Grid.SetRow(TabsHeaderContainer, 0);

        TabContentPresenter = new ContentPresenter {
            Padding = new Thickness(10)
        };
        Grid.SetRow(TabContentPresenter, 1);

        var mainGrid = new Grid {
            RowDefinitions = new RowDefinitionCollection
            {
                new RowDefinition { Height = GridLength.Auto },
                new RowDefinition { Height = GridLength.Star }
            }
        };

        mainGrid.Children.Add(TabsHeaderContainer);
        mainGrid.Children.Add(TabContentPresenter);

        Content = mainGrid;

        ItemSource.CollectionChanged += OnItemsChanged;
    }

    public void AddTab(TabItem item)
    {
        if (item != null) {
            ItemSource.Add(item);
        }
    }

    private void OnItemsChanged(object sender, NotifyCollectionChangedEventArgs e)
    {
        BuildTabs();
    }

    private void BuildTabs()
    {
        foreach (var child in TabsHeaderContainer.Children) {
            if (child is Button button) {
                button.Clicked -= OnTabButtonClicked;
            }
        }
        TabsHeaderContainer.Children.Clear();

        foreach (var item in ItemSource) {
            var button = new Button {
                Margin = new Thickness(2),
                Padding = new Thickness(15, 5),
                BindingContext = item
            };

            button.SetBinding(Button.TextProperty, new Binding(nameof(TabItem.HeaderText), source: item));

            button.Clicked += OnTabButtonClicked;

            TabsHeaderContainer.Children.Add(button);
        }

        if (ItemSource.Count > 0) {
            if (SelectedTabIndex >= 0 && SelectedTabIndex < ItemSource.Count) {
                SelectTab(ItemSource[SelectedTabIndex]);
            } else {
                SelectedTabIndex = 0;
            }
        } else {
            TabContentPresenter.Content = null;
        }
    }

    private void OnTabButtonClicked(object sender, EventArgs e)
    {
        if (sender is Button button && button.BindingContext is TabItem clickedItem) {
            int index = ItemSource.IndexOf(clickedItem);
            if (index != -1) {
                SelectedTabIndex = index;
            }
        }
    }

    private static void OnSelectedTabIndexChanged(BindableObject bindable, object oldValue, object newValue)
    {
        if (bindable is TabViewControl control && newValue is int newIndex) {
            if (newIndex >= 0 && newIndex < control.ItemSource.Count) {
                control.SelectTab(control.ItemSource[newIndex]);
            }
        }
    }

    private void SelectTab(TabItem selectedItem)
    {
        if (TabContentPresenter.Content != selectedItem) {
            TabContentPresenter.Content = selectedItem;
            PositionChanged?.Invoke(this, EventArgs.Empty);
        }

        UpdateTabVisuals();
    }

    private void UpdateTabVisuals()
    {
        if (SelectedTabIndex < 0 || SelectedTabIndex >= ItemSource.Count) return;

        var selectedItem = ItemSource[SelectedTabIndex];

        foreach (var child in TabsHeaderContainer.Children) {
            if (child is Button btn) {
                bool isCurrent = btn.BindingContext == selectedItem;
                btn.BackgroundColor = isCurrent ? SelectedTabColor : UnselectedTabColor;
                btn.TextColor = TabTextColor;
            }
        }
    }
}
