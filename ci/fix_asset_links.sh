#!/usr/bin/env bash

tag=
token=

read -sp 'Access Token ' token
echo
read -p 'Release Tag: ' tag

response=$(curl \
  --request GET \
  --header "PRIVATE-TOKEN: ${token}" \
  "https://git.ps.informatik.uni-kiel.de/api/v4/projects/88/releases/${tag}/assets/links/")

# get id, name and url for each link and put it into an array
tmp=$(echo "${response}" | jq -r '.[] | .id ')
ids=($tmp)
tmp=$(echo "${response}" | jq -r '.[] | .name ')
names=($tmp)
tmp=$(echo "${response}" | jq -r '.[] | .url ')
old_urls=($tmp)

# fix each link
for idx in "${!ids[@]}" ; do

    echo

    link_id="${ids[idx]}"
    name="${names[idx]}"
    old_url="${old_urls[idx]}"

    echo "Fixing Link with ID: ${link_id}"
    echo "Corresponding Name: ${name}"
    echo "Current URL: ${old_url}"

    unset url
    read -p "New URL [Current URL]: " url
    url=${url:-${old_url}}

    curl \
      --request PUT \
      --data name="${name}" \
      --data filepath="/other/${name}" \
      --data url="${url}" \
      --header "PRIVATE-TOKEN: ${token}" \
      "https://git.ps.informatik.uni-kiel.de/api/v4/projects/88/releases/${tag}/assets/links/${link_id}"

done

echo
