# Справочник MCP-инструментов GKcli

## Обзор

**GKcli** — это CLI-приложение GEDKeeper с поддержкой протокола [MCP](https://modelcontextprotocol.io/) (Model Context Protocol).
MCP-сервер позволяет LLM-клиентам (LM Studio, Jan и др.) взаимодействовать с генеалогической базой данных через набор инструментов.

**Запуск в LLM-клиентах:** `GKcli --mcp` (аргумент обязателен, т.к. без него запускается интерактивный текстовый терминал, частично повторяющий по составу функций MCP-сервер).

**Протокол:** MCP `2025-06-18`, JSON-RPC 2.0 поверх stdin/stdout. Без внешних зависимостей.

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
| `file_search` | Найти все GEDCOM-файлы на диске | `path` (string) — корневой каталог для поиска |
| `file_validate` | Проверить валидность текущей базы | — |

---

## Записи (общее)

| Инструмент | Описание | Параметры |
|---|---|---|
| `record_search` | Нечёткий поиск по любым записям (имя/заголовок) | `record_type` (string), `search_text` (string), `threshold` (number, по умолч. 0.15) |
| `record_list_userrefs` | Список пользовательских сносок записи | `record_xref` (string) |
| `record_add_userref` | Добавить пользовательскую сноску к записи | `record_xref` (string), `string_value` (string), `reference_type` (string, необязательно) |
| `record_delete_userref` | Удалить пользовательскую сноску из записи | `record_xref` (string), `reference_index` (integer, 0-based) |
| `record_list_sources` | Список цитат источников записи | `record_xref` (string) |
| `record_add_source` | Добавить цитату источника к записи | `record_xref` (string), `source_xref` (string), `page` (string, необязательно), `certainty` (integer, 0–3, необязательно) |
| `record_delete_source` | Удалить цитату источника из записи | `record_xref` (string), `citation_index` (integer, 0-based) |
| `record_list_multimedia` | Список мультимедиа-объектов записи | `record_xref` (string) |
| `record_add_multimedia` | Добавить ссылку на мультимедиа к записи | `record_xref` (string), `multimedia_xref` (string), `is_primary` (boolean, по умолч. false) |
| `record_delete_multimedia` | Удалить ссылку на мультимедиа из записи | `record_xref` (string), `link_index` (integer, 0-based) |
| `record_list_notes` | Список заметок записи | `record_xref` (string) |
| `record_add_note` | Добавить ссылку на заметку к записи | `record_xref` (string), `note_xref` (string) |
| `record_delete_note` | Удалить ссылку на заметку из записи | `record_xref` (string), `note_index` (integer, 0-based) |

**Типы записей:** `Individual`, `Family`, `Note`, `Source`, `Repository`, `Multimedia`, `Group`, `Task`, `Research`, `Communication`, `Location`

---

## Персоны (Individuals)

| Инструмент | Описание | Параметры |
|---|---|---|
| `individual_list` | Список всех персон (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `individual_search` | Нечёткий поиск по имени (порог 16%) | `name` (string) |
| `individual_add` | Добавить персону | `name` (string), `sex` (string: `m`/`f`), `nickname` (string, необязательно) |
| `individual_delete` | Удалить персону | `xref` (string, напр. `I1`) |
| `individual_list_associations` | Список всех ассоциаций персоны | `individual_xref` (string) |
| `individual_add_association` | Добавить ассоциацию (связь) между двумя персонами | `individual_xref` (string), `associate_xref` (string), `relation` (string) |
| `individual_delete_association` | Удалить ассоциацию у персоны | `individual_xref` (string), `association_index` (integer, 0-based) |
| `individual_list_events` | Список всех событий персоны | `individual_xref` (string) |
| `individual_delete_event` | Удалить событие персоны | `individual_xref` (string), `event_index` (integer, 0-based) |
| `individual_add_event` | Добавить событие персоне | `individual_xref` (string), `tag` (string), `type` (string, optional), `date` (string, dd/mm/yyyy), `place` (string), `location_xref` (string), `cause` (string), `agency` (string), `value` (string) |
| `individual_list_event_types` | Список всех доступных типов событий для персон | — |

---

## Семьи (Families)

| Инструмент | Описание | Параметры |
|---|---|---|
| `family_list` | Список всех семей (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `family_add` | Создать семью | `husband_xref` (string), `wife_xref` (string) |
| `family_delete` | Удалить семью | `xref` (string, напр. `F1`) |
| `family_list_children` | Список детей семьи | `family_xref` (string) |
| `family_add_child` | Добавить ребёнка в семью | `family_xref` (string), `child_xref` (string) |
| `family_delete_child` | Удалить ребёнка из семьи | `family_xref` (string), `child_xref` (string) |
| `family_list_events` | Список всех событий семьи | `family_xref` (string) |
| `family_delete_event` | Удалить событие семьи | `family_xref` (string), `event_index` (integer, 0-based) |
| `family_add_event` | Добавить событие семье | `family_xref` (string), `tag` (string), `type` (string, optional), `date` (string, dd/mm/yyyy), `place` (string), `location_xref` (string), `cause` (string), `agency` (string), `value` (string) |
| `family_list_event_types` | Список всех доступных типов событий для семей | — |

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
| `repository_list` | Список всех хранилищ (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `repository_add` | Добавить хранилище | `name` (string) |
| `repository_delete` | Удалить хранилище | `xref` (string, напр. `R1`) |

---

## Мультимедиа (Multimedia)

| Инструмент | Описание | Параметры |
|---|---|---|
| `multimedia_list` | Список мультимедиа-записей (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `multimedia_add` | Добавить мультимедиа-запись с файловой ссылкой | `title` (string) — название, `file_path` (string) — путь к файлу или URL, `media_type` (string — `Unknown`, `Audio`, `Book`, `Card`, `Electronic`, `Fiche`, `Film`, `Magazine`, `Manuscript`, `Map`, `Newspaper`, `Photo`, `Tombstone`, `Video`), `store_type` (string — `Reference`, `RelativeReference`, `Archive`, `URL`) |
| `multimedia_delete` | Удалить мультимедиа-запись | `xref` (string, напр. `M1`) |

---

## Группы (Groups)

| Инструмент | Описание | Параметры |
|---|---|---|
| `group_list` | Список групп (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `group_add` | Создать группу | `name` (string) |
| `group_delete` | Удалить группу | `xref` (string, напр. `G1`) |
| `group_list_members` | Список членов группы | `group_xref` (string) |
| `group_add_member` | Добавить персону в группу | `group_xref` (string), `individual_xref` (string) |
| `group_delete_member` | Удалить персону из группы | `group_xref` (string), `individual_xref` (string) |

---

## Задачи (Tasks)

| Инструмент | Описание | Параметры |
|---|---|---|
| `task_list` | Список задач (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `task_delete` | Удалить задачу | `xref` (string, напр. `TSK1`) |

---

## Исследования (Researches)

| Инструмент | Описание | Параметры |
|---|---|---|
| `research_list` | Список исследований (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `research_delete` | Удалить исследование | `xref` (string, напр. `RES1`) |

---

## Переписка (Communications)

| Инструмент | Описание | Параметры |
|---|---|---|
| `communication_list` | Список записей переписки (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `communication_delete` | Удалить запись переписки | `xref` (string, напр. `COMM1`) |

---

## Местоположения (Locations)

| Инструмент | Описание | Параметры |
|---|---|---|
| `location_list` | Список местоположений (пагинация, 20 на стр.) | `page` (integer, по умолч. 1) |
| `location_delete` | Удалить местоположение | `xref` (string, напр. `LOC1`) |

---

## Особенности реализации

- **Автосоздание бэкапов:** при работе через MCP автоматически включается принудительное резервное копирование каждого файла при сохранении.
- **Пагинация:** таблицы с >20 записей разбиваются постранично; в конце ответа подсказка для запроса следующей страницы.
- **Нечёткий поиск:** `individual_search` и `record_search` используют алгоритм нечёткого сопоставления с настраиваемым порогом.
- **Логирование:** все сообщения пишутся в лог с префиксом `[GKcli MCP]`.
- **Keep-alive:** сервер отправляет периодические heartbeat-сигналы каждые 30 секунд.
