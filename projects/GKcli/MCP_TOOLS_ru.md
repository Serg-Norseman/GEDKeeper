# Справочник MCP-инструментов GKcli

## Обзор

**GKcli** — это CLI-приложение GEDKeeper с поддержкой протокола [MCP](https://modelcontextprotocol.io/) (Model Context Protocol).
MCP-сервер позволяет LLM-клиентам (LM Studio, Jan и др.) взаимодействовать с генеалогической базой данных через набор инструментов.

**Запуск в LLM-клиентах:** `GKcli --mcp`

**Протокол:** MCP `2025-06-18`, JSON-RPC 2.0 поверх stdin/stdout. Без внешних зависимостей — только `System.Text.Json`.

**Внимание:** не рекомендуется для тестирования использовать LM Studio (пока не будет удалено это предупреждение),
т.к. в этой программе каждые 1-2 минуты перезапускается новый экземпляр MCP-сервера, что приводит к непрерывным сбросам контекста БД.
Пока успешное функционирование подтверждено только для Jan.

---

## Файловые операции

| Инструмент | Описание | Параметры |
|---|---|---|
| `file_new` | Создать новую пустую базу GEDCOM | — |
| `file_load` | Загрузить GEDCOM-файл | `path` (string) — путь к `.ged` |
| `file_save` | Сохранить базу в GEDCOM-файл | `path` (string) — путь для сохранения |
| `file_props` | Получить информацию о текущей базе (автор, адрес, статистика записей) | — |
| `file_recent` | Список недавно открытых файлов | — |
| `file_reload` | Перезагрузить последний открытый файл | — |
| `file_search` | Найти все GEDCOM-файлы на диске (`d:/`) | — |
| `file_validate` | Проверить валидность текущей базы | — |

---

## Записи (общее)

| Инструмент | Описание | Параметры |
|---|---|---|
| `record_search` | Нечёткий поиск по любым записям (имя/заголовок) | `record_type` (string), `search_text` (string), `threshold` (number, по умолч. 0.15) |
| `record_add_userref` | Добавить пользовательскую сноску к записи | `record_xref` (string), `string_value` (string), `reference_type` (string, необязательно) |
| `record_delete_userref` | Удалить пользовательскую сноску из записи | `record_xref` (string), `reference_index` (integer, 0-based) |
| `record_add_source` | Добавить цитату источника к записи | `record_xref` (string), `source_xref` (string), `page` (string, необязательно), `certainty` (integer, 0–3, необязательно) |
| `record_delete_source` | Удалить цитату источника из записи | `record_xref` (string), `citation_index` (integer, 0-based) |

**Типы записей:** `Individual`, `Family`, `Note`, `Source`, `Repository`, `Multimedia`, `Group`, `Task`, `Research`, `Communication`, `Location`

---

## Персоны (Individuals)

| Инструмент | Описание | Параметры |
|---|---|---|
| `individual_list` | Список всех персон (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `individual_search` | Нечёткий поиск по имени (порог 16%) | `name` (string) |
| `individual_add` | Добавить персону | `name` (string), `sex` (string: `m`/`f`) |
| `individual_delete` | Удалить персону | `xref` (string, напр. `I1`) |

---

## Семьи (Families)

| Инструмент | Описание | Параметры |
|---|---|---|
| `family_list` | Список всех семей (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `family_add` | Создать семью | `husband_xref` (string), `wife_xref` (string) |
| `family_add_child` | Добавить ребёнка в семью | `family_xref` (string), `child_xref` (string) |
| `family_delete_child` | Удалить ребёнка из семьи | `family_xref` (string), `child_xref` (string) |
| `family_delete` | Удалить семью | `xref` (string, напр. `F1`) |

---

## Заметки (Notes)

| Инструмент | Описание | Параметры |
|---|---|---|
| `note_list` | Список заметок (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `note_add` | Добавить заметку | `text` (string) |
| `note_delete` | Удалить заметку | `xref` (string, напр. `N1`) |

---

## Источники (Sources)

| Инструмент | Описание | Параметры |
|---|---|---|
| `source_list` | Список источников (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `source_add` | Добавить источник | `title` (string) |
| `source_delete` | Удалить источник | `xref` (string, напр. `S1`) |

---

## Хранилища (Repositories)

| Инструмент | Описание | Параметры |
|---|---|---|
| `repository_list` | Список всех хранилищ | — |
| `repository_add` | Добавить хранилище | `name` (string) |
| `repository_delete` | Удалить хранилище | `xref` (string, напр. `R1`) |

---

## Мультимедиа (Multimedia)

| Инструмент | Описание | Параметры |
|---|---|---|
| `multimedia_list` | Список мультимедиа-записей (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |

---

## Группы (Groups)

| Инструмент | Описание | Параметры |
|---|---|---|
| `group_list` | Список групп | — |
| `group_add` | Создать группу | `name` (string) |
| `group_add_member` | Добавить персону в группу | `group_xref` (string), `individual_xref` (string) |
| `group_delete_member` | Удалить персону из группы | `group_xref` (string), `individual_xref` (string) |
| `group_delete` | Удалить группу | `xref` (string, напр. `G1`) |

---

## Задачи (Tasks)

| Инструмент | Описание | Параметры |
|---|---|---|
| `task_list` | Список задач (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |

---

## Исследования (Researches)

| Инструмент | Описание | Параметры |
|---|---|---|
| `research_list` | Список исследований (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |

---

## Переписка (Communications)

| Инструмент | Описание | Параметры |
|---|---|---|
| `communication_list` | Список записей переписки (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |

---

## Местоположения (Locations)

| Инструмент | Описание | Параметры |
|---|---|---|
| `location_list` | Список местоположений (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |

---

## Особенности реализации

- **Автосоздание бэкапов:** при работе через MCP автоматически включается принудительное резервное копирование каждого файла при сохранении.
- **Пагинация:** таблицы с >20 записей разбиваются постранично; в конце ответа подсказка для запроса следующей страницы.
- **Нечёткий поиск:** `individual_search` и `record_search` используют алгоритм нечёткого сопоставления с настраиваемым порогом.
- **Логирование:** все сообщения пишутся в лог с префиксом `[GKcli MCP]`.
- **Keep-alive:** сервер отправляет периодические heartbeat-сигналы каждые 30 секунд.
