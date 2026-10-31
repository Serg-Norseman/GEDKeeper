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

    public int SelectedTabIndex { get; set; }


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

    }

    private void OnItemsChanged(object sender, NotifyCollectionChangedEventArgs e)
    {
        BuildTabs();
    }

    private void BuildTabs()
    {
        TabsHeaderContainer.Children.Clear();

        foreach (var item in ItemSource) {
            var button = new Button {
                Text = item.HeaderText,
                Margin = new Thickness(2),
                Padding = new Thickness(15, 5),
                BindingContext = item
            };

            button.Clicked += (s, e) => SelectTab(item);

            TabsHeaderContainer.Children.Add(button);
        }

        if (ItemSource.Count > 0 && TabContentPresenter.Content == null) {
            SelectTab(ItemSource[0]);
        }
    }

    private void SelectTab(TabItem selectedItem)
    {
        TabContentPresenter.Content = selectedItem;

        foreach (Button btn in TabsHeaderContainer.Children) {
            bool isCurrent = btn.BindingContext == selectedItem;
            btn.BackgroundColor = isCurrent ? Colors.DeepSkyBlue : Colors.LightGray;
            btn.TextColor = isCurrent ? Colors.White : Colors.Black;
        }
    }
}
