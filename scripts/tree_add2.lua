
i1 = create_individual("Иван", "Иванович", "Петров", "M");
i2 = create_individual("Марья", "Сидоровна", "Петрова", "F");

f = create_family();

bind_family_spouse(f, i1);
bind_family_spouse(f, i2);

n = create_note();
bind_record_note(i1, n);
add_note_text(n, "Пример заметки");
add_note_text(n, "Строка 2");
add_note_text(n, "Строка 3");

update_view();