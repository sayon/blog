all:
	bundle exec jekyll build --trace

serve:
	bundle exec jekyll serve

auto:
	bundle exec jekyll build --watch
clean:
	rm -rf _site/*

publish:
	ssh blog@rubber-duck-typing.com 'cd www; git pull && make '
