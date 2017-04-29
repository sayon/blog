all:
	bundle exec jekyll build --trace

serve:
	bundle exec jekyll serve

auto:
	bundle exec jekyll build --watch
clean:
	rm -rf _site/*
