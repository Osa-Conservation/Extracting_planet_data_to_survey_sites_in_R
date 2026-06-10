# Getting this repo onto GitHub with Pages enabled

A 5-minute one-time setup. Do this **after** you've rotated your Sentinel Hub secret and confirmed nothing in `R/` contains real credentials.

## 1. Create the repo locally

```bash
cd osa-planet-r-tutorial

git init
git add .
git commit -m "Initial commit: Planet data in R tutorial"
```

## 2. Create the empty repo on GitHub

Either via the web UI at <https://github.com/new>, or with the `gh` CLI:

```bash
gh repo create osa-planet-r-tutorial --public --source=. --remote=origin --push
```

If you're using the web UI:

```bash
git remote add origin https://github.com/YOUR-GH-USERNAME/osa-planet-r-tutorial.git
git branch -M main
git push -u origin main
```

## 3. Turn on GitHub Pages

In the repo on GitHub:

1. **Settings** → **Pages**
2. **Source:** "Deploy from a branch"
3. **Branch:** `main`, folder: `/ (root)`
4. Save.

Wait ~30 seconds. Your site will be live at:

```
https://YOUR-GH-USERNAME.github.io/osa-planet-r-tutorial/
```

It serves `index.md` as the landing page. The Cayman theme is configured in `_config.yml` — change `theme:` to any [supported theme](https://pages.github.com/themes/) (minima, minimal, architect, slate) if Cayman isn't to your taste.

## 4. Update the URLs

A few placeholders in the source files need replacing once the repo exists:

```bash
# macOS / Linux — replace YOUR-GH-USERNAME everywhere
grep -rl YOUR-GH-USERNAME . --exclude-dir=.git | \
  xargs sed -i.bak 's/YOUR-GH-USERNAME/your-actual-username/g'

# then commit
git add . && git commit -m "Update repo URLs" && git push
```

## 5. Sanity-check before the session

- [ ] Site loads at the GitHub Pages URL.
- [ ] Code blocks render with syntax highlighting.
- [ ] Links to `R/*.R` files resolve.
- [ ] `R/00_setup.R` runs cleanly on a fresh machine.
- [ ] `.Renviron` is NOT in the repo (`git ls-files | grep -i renviron` should be empty).
- [ ] `grep -r client_secret R/` returns only the `Sys.getenv` reference, never a literal value.

That last one is worth doing every time you push.
