"""Tests for news.py. Run: python3 -m unittest discover -s .github/scripts -p 'test_news.py'"""
import unittest

from news import check, collect, merge_order, ships_nothing

IGNORE = [r"^\.github$", r"^\.claude$", r"^AGENTS\.md$", r"^news$"]


class ShipsNothing(unittest.TestCase):
    def test_ignored_dirs_and_rbuildignore_itself(self):
        self.assertTrue(ships_nothing(
            [".github/workflows/x.yaml", ".Rbuildignore", "AGENTS.md"], IGNORE))

    def test_shipped_file(self):
        self.assertFalse(ships_nothing(["R/a.R", ".github/x"], IGNORE))

    def test_fails_closed_on_empty_inputs(self):
        self.assertFalse(ships_nothing([], IGNORE))
        self.assertFalse(ships_nothing(["AGENTS.md"], []))


class Check(unittest.TestCase):
    def test_shipping_change_without_fragment_fails(self):
        self.assertTrue(check(["R/a.R"], {}, IGNORE, "1.0.0", "1.0.0"))

    def test_shipping_change_with_fragment_passes(self):
        self.assertEqual(check(["R/a.R", "news/fix-a.md"],
                               {"news/fix-a.md": "* Fixed a.\n"},
                               IGNORE, "1.0.0", "1.0.0"), [])

    def test_empty_fragment_fails(self):
        self.assertTrue(check(["R/a.R", "news/fix-a.md"],
                              {"news/fix-a.md": "\n"}, IGNORE, "1.0.0", "1.0.0"))

    def test_fragment_outside_news_root_does_not_count(self):
        self.assertTrue(check(["R/a.R"], {"news/sub/x.md": "* x\n"},
                              IGNORE, "1.0.0", "1.0.0"))

    def test_ships_nothing_needs_no_fragment(self):
        self.assertEqual(check([".github/x.yaml"], {}, IGNORE, "1.0.0", "1.0.0"), [])

    def test_bump_passes(self):
        self.assertEqual(check(["DESCRIPTION", "NEWS.md", "news/a.md"], {},
                               IGNORE, "1.0.0", "1.0.1"), [])

    def test_collect_without_a_version_move_passes(self):
        # ggRandomForests files fragments into its existing (development)
        # section without moving Version:; the collect deletes them.
        self.assertEqual(check(["NEWS.md", "news/a.md"], {}, IGNORE,
                               "4.0.0", "4.0.0", ["news/a.md"]), [])

    def test_collect_that_also_ships_code_still_needs_a_fragment(self):
        # A collect passes on what it consumes, not on what rides along.
        self.assertTrue(check(["NEWS.md", "news/a.md", "R/a.R"], {}, IGNORE,
                              "4.0.0", "4.0.0", ["news/a.md"]))

    def test_deleting_a_fragment_without_touching_news_is_not_a_collect(self):
        # A stray deletion must not let a shipping change skip its own entry.
        self.assertTrue(check(["R/a.R", "news/b.md"], {}, IGNORE,
                              "1.0.0", "1.0.0", ["news/b.md"]))

    def test_deleting_something_else_is_not_a_collect(self):
        self.assertTrue(check(["R/a.R", "R/b.R"], {}, IGNORE,
                              "1.0.0", "1.0.0", ["R/b.R"]))

    def test_missing_rbuildignore_fails_closed(self):
        self.assertTrue(check([".github/x.yaml"], {}, [], "1.0.0", "1.0.0"))


class MergeOrder(unittest.TestCase):
    def test_oldest_add_first_and_untracked_last(self):
        log = ["news/c.md", "news/b.md", "news/a.md"]  # newest first
        self.assertEqual(merge_order(log, ["news/a.md", "news/b.md", "news/z.md"]),
                         ["news/a.md", "news/b.md", "news/z.md"])

    def test_recycled_name_takes_its_newest_add(self):
        log = ["news/a.md", "news/b.md", "news/a.md"]
        self.assertEqual(merge_order(log, ["news/a.md", "news/b.md"]),
                         ["news/b.md", "news/a.md"])


class Collect(unittest.TestCase):
    def test_new_heading_above_newest_release(self):
        news = "# pkg 1.0.0\n\n* Old.\n"
        self.assertEqual(collect(news, "pkg", "1.0.1", ["* A.\n", "* B.\n"]),
                         "# pkg 1.0.1\n\n* A.\n\n* B.\n\n# pkg 1.0.0\n\n* Old.\n")

    def test_folds_unreleased_wherever_it_sits(self):
        news = ("# pkg 1.0.0\n\n* Old.\n\n# pkg (unreleased)\n\n* Legacy.\n\n"
                "# pkg 0.9.0\n\n* Older.\n")
        self.assertEqual(collect(news, "pkg", "1.0.1", ["* New.\n"]),
                         "# pkg 1.0.1\n\n* Legacy.\n\n* New.\n\n# pkg 1.0.0\n\n"
                         "* Old.\n\n# pkg 0.9.0\n\n* Older.\n")

    def test_dcf_preamble_version_moves(self):
        news = "Package: pkg\nVersion: 1.0.0\n\n# pkg 1.0.0\n\n* Old.\n"
        out = collect(news, "pkg", "1.0.1", ["* A.\n"])
        self.assertTrue(out.startswith("Package: pkg\nVersion: 1.0.1\n\n# pkg 1.0.1\n"))

    def test_appends_to_existing_setext_development_section(self):
        news = ("Package: pkg\nVersion: 4.0.0\n\npkg v4.0.0 (development)\n"
                "========================\n* Earlier.\n\npkg v3.9.0\n"
                "==========\n* Old.\n")
        out = collect(news, "pkg", "4.0.0", ["* A.\n"])
        self.assertIn("========================\n\n* Earlier.\n\n* A.\n\npkg v3.9.0", out)

    def test_new_heading_follows_setext_style(self):
        news = "pkg v3.9.0\n==========\n* Old.\n"
        self.assertTrue(collect(news, "pkg", "4.0.0", ["* A.\n"])
                        .startswith("pkg 4.0.0\n=========\n\n* A.\n"))

    def test_version_match_is_not_a_prefix_match(self):
        news = "# pkg 1.0.10\n\n* Old.\n"
        self.assertTrue(collect(news, "pkg", "1.0.1", ["* A.\n"])
                        .startswith("# pkg 1.0.1\n\n* A.\n\n# pkg 1.0.10\n"))

    def test_comment_in_code_fence_is_not_a_heading(self):
        news = "# pkg 1.0.0\n\n* Old:\n\n  ```r\n# pkg (unreleased)\n  ```\n"
        self.assertEqual(collect(news, "pkg", "1.0.1", ["* A.\n"]),
                         "# pkg 1.0.1\n\n* A.\n\n" + news)

    def test_nothing_to_collect_raises(self):
        with self.assertRaises(ValueError):
            collect("# pkg 1.0.0\n\n* Old.\n", "pkg", "1.0.1", [])

    def test_subsections_stay_inside_their_release(self):
        news = "# pkg 1.0.0\n\n## Breaking changes\n\n* Old.\n"
        self.assertEqual(collect(news, "pkg", "1.0.1", ["* A.\n"]),
                         "# pkg 1.0.1\n\n* A.\n\n# pkg 1.0.0\n\n## Breaking changes\n\n* Old.\n")


if __name__ == "__main__":
    unittest.main()
